{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (
    main
    ) where

import           Data.Function         (on)
import           Data.List             hiding (intercalate, unwords)
import           Data.Maybe            (mapMaybe)
import           Data.Text             hiding (concat, filter, groupBy, map, concatMap)
import           Data.Text.IO          (putStr, putStrLn)
import           Database.MySQL.Simple
import           Options.Generic
import           Prelude               hiding (putStr, putStrLn, unwords)

data SqlType = SqlText | SqlInt | SqlLong | SqlBool | SqlNumeric | SqlByteArray | SqlDate | SqlTime | SqlOther | SqlMaybe SqlType

data HaskellType = HaskText | HaskInt | HaskLong | HaskBool | HaskNumeric | HaskByteArray | HaskDate | HaskTime | HaskUnknown | HaskMaybe HaskellType

data Table = Table {
    tname :: Text,
    cols  :: [Column]
}

data Column = Column {
    cname :: Text,
    ctyp  :: SqlType,
    dflt  :: Maybe Text,
    iskey :: Bool
}

data Record = Record {
    hname  :: Text,
    fields :: [Field]
}

data Field = Field {
    fname :: Text,
    ftyp  :: HaskellType
}

haskFromSql SqlText = HaskText
haskFromSql SqlInt = HaskInt
haskFromSql SqlLong = HaskLong
haskFromSql SqlBool = HaskBool
haskFromSql SqlNumeric = HaskNumeric
haskFromSql SqlByteArray = HaskByteArray
haskFromSql SqlDate = HaskDate
haskFromSql SqlTime = HaskTime
haskFromSql SqlOther = HaskUnknown
haskFromSql (SqlMaybe t) = HaskMaybe $ haskFromSql t

typeToStr HaskText = "!Text"
typeToStr HaskInt = "!Int"
typeToStr HaskLong = "!Integer"
typeToStr HaskBool = "!Bool"
typeToStr HaskNumeric = "!Decimal"
typeToStr HaskByteArray = "!ByteArray"
typeToStr HaskDate = "!Day"
typeToStr HaskTime = "!UTCTime"
typeToStr HaskUnknown = "!Unknown"
typeToStr (HaskMaybe t) = append "!Maybe " $ typeToStr t

convertSqlType s isnull
      | isnull == "YES" = SqlMaybe $ convertSqlType s ""
      | s `elem` ["text", "varchar", "char"] = SqlText
      | s `elem` ["integer", "int", "smallint", "tinyint"] = SqlInt
      | s `elem` ["bigint", "long"] = SqlLong
      | s `elem` ["money", "numeric", "decimal"] = SqlNumeric
      | s `elem` ["bit", "bool", "boolean"] = SqlBool
      | s `elem` ["image", "blob", "longblob"] = SqlByteArray
      | s `elem` ["date"] = SqlDate
      | s `elem` ["datetime", "time", "timestamp"] = SqlTime
      | otherwise = SqlOther

imports :: HaskellType -> Maybe Text
imports HaskTime = Just "Data.Time.Clock"
imports HaskDate = Just "Data.Time.Calendar"
imports HaskNumeric = Just "Data.Decimal"
imports _ = Nothing


-- Configuration
data Conf = Conf {
    password :: String,
    server   :: String,
    user     :: String,
    database :: String,
    package  :: Text
} deriving (Generic, Show)

instance ParseRecord Conf

-- Result type used for the query
type Result = (Text, Text, Text, Text, Text, Maybe Text)

tableName (t, _, _, _, _, _) = t

groupResultByTable :: [Result] -> [[Result]]
groupResultByTable = groupBy ( (==) `on` tableName)

extractTables :: Conf -> IO [Table]
extractTables conf = do
    conn <- connect defaultConnectInfo { connectDatabase = "information_schema",
        connectPassword = password conf,
        connectUser = user conf,
        connectHost = server conf }
    let q =  "Select TABLE_NAME, COLUMN_NAME, DATA_TYPE, COLUMN_KEY, IS_NULLABLE, COLUMN_DEFAULT FROM COLUMNS WHERE TABLE_SCHEMA = ?"
    xs <- query conn q [database conf] :: IO [Result]
    let gxs = groupResultByTable xs
        tables = map extractTable gxs
    return tables

extractTable :: [Result] -> Table
extractTable res@(x:_) = Table (tableName x) (map extractColumn res)

extractColumn :: Result -> Column
extractColumn (_, cname', dtype', iskey', isnull', def') = Column cname' (convertSqlType dtype' isnull') def' (iskey' == "YES")

tableToRecord :: Table -> Record
tableToRecord t = Record (toTitle (tname t)) fields
            where
                fields = fmap columnToField (cols t)

columnToField :: Column -> Field
columnToField c = Field (append "_" (toCaseFold cname')) haskTyp
    where
        cname' = cname c
        haskTyp = haskFromSql (ctyp c)

printRecords :: Conf -> [Record] -> IO ()
printRecords conf recs = do
    putStrLn "{-# DeriveGeneric #-}\n"
    putStrLn $ unwords ["module", package conf, "("]
    putStrLn $ intercalate "\n," recNames
    putStrLn ") where\n"
    printImports recs
    mapM_ printRec recs
    where
        recNames = map (flip append "(..)" . hname) recs

printImports :: [Record] -> IO ()
printImports recs = do
    let flds = concatMap fields recs
        mimps = mapMaybe (imports . ftyp) flds
        imps = map (append "import    ") mimps
    mapM_ putStrLn imps

printRec :: Record -> IO ()
printRec r = do
    putStr $ unwords [ "data", recName, "=", recName, "{\n    "]
    putStrLn $ intercalate "\n  , " fldsStr
    putStrLn $ unwords ["} deriving (Generic, Show)\n\ninstance ToJSON", recName, "\n"]
    where
        recName = hname r
        flds = fields r
        fldsStr = map fieldLine flds

fieldLine :: Field -> Text
fieldLine f = unwords [fname f, "::", typeToStr (ftyp f)]

main = do
    conf <- getRecord "Configuration"
    tables <- extractTables (conf :: Conf)
    let records = fmap tableToRecord tables
    printRecords conf records
    return ()
