{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (
    main
    ) where

import           Data.Function
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

data HaskellRecord = HaskellRecord {
    hname  :: Text,
    fields :: [Field]
}

data Field = Field {
    fname :: Text,
    ftyp  :: HaskellType
}

haskTypeFromType SqlText = HaskText
haskTypeFromType SqlInt = HaskInt
haskTypeFromType SqlLong = HaskLong
haskTypeFromType SqlBool = HaskBool
haskTypeFromType SqlNumeric = HaskNumeric
haskTypeFromType SqlByteArray = HaskByteArray
haskTypeFromType SqlDate = HaskDate
haskTypeFromType SqlTime = HaskTime
haskTypeFromType SqlOther = HaskUnknown
haskTypeFromType (SqlMaybe t) = HaskMaybe $ haskTypeFromType t

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

typeFromSql s isnull
      | isnull == "YES" = SqlMaybe $ typeFromSql s ""
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

data Conf = Conf {
    password :: String,
    server   :: String,
    user     :: String,
    database :: String,
    package  :: Text
} deriving (Generic, Show)

instance ParseRecord Conf

tableName (t, _, _, _, _, _) = t

type Result = (Text, Text, Text, Text, Text, Maybe Text)

groupByTable :: [Result] -> [[Result]]
groupByTable = groupBy ( (==) `on` tableName)

extractTables :: Conf -> IO [Table]
extractTables conf = do
    conn <- connect defaultConnectInfo { connectDatabase = "information_schema",
        connectPassword = password conf,
        connectUser = user conf,
        connectHost = server conf }
    let q =  "Select TABLE_NAME, COLUMN_NAME, DATA_TYPE, COLUMN_KEY, IS_NULLABLE, COLUMN_DEFAULT FROM COLUMNS WHERE TABLE_SCHEMA = ?"
    xs <- query conn q [database conf] :: IO [Result]
    let gxs = groupByTable xs
        tables = map extractTable gxs
    return tables

extractTable :: [Result] -> Table
extractTable res@(x:_) = Table (tableName x) (map extractColumn res)

extractColumn :: Result -> Column
extractColumn (_, cname', dtype', iskey', isnull', def') = Column cname' (typeFromSql dtype' isnull') def' (iskey' == "YES")

main = do
    conf <- getRecord "Configuration"
    tables <- extractTables (conf :: Conf)
    let records = fmap tableToRecord tables
    printRecords conf records
    return ()

tableToRecord :: Table -> HaskellRecord
tableToRecord t = HaskellRecord (toTitle (tname t)) fields
            where
                fields = fmap columnToField (cols t)

columnToField :: Column -> Field
columnToField c = Field (append "_" (toCaseFold cname')) haskTyp
    where
        cname' = cname c
        haskTyp = haskTypeFromType (ctyp c)

printRecords :: Conf -> [HaskellRecord] -> IO ()
printRecords conf recs = do
    putStrLn "{-# DeriveGeneric #-}\n"
    putStrLn $ unwords ["module", package conf, "("]
    putStrLn $ intercalate "\n," recNames
    putStrLn ") where\n"
    printImports recs
    mapM_ printRec recs
    where
        recNames = map (flip append "(..)" . hname) recs

printImports :: [HaskellRecord] -> IO ()
printImports recs = do
    let flds = concatMap fields recs
        mimps = mapMaybe (imports . ftyp) flds
        imps = map (append "import    ") mimps
    mapM_ putStrLn imps

printRec :: HaskellRecord -> IO ()
printRec r = do
    putStr $ unwords [ "data", recName, "=", recName, "{\n    "]
    putStrLn $ intercalate "\n  , " fldsStr
    putStrLn $ unwords ["} deriving (Generic, Show)\n\ninstance ToJSON", recName, "\n"]
    where
        recName = hname r
        flds = fields r
        fldsStr = map printField flds

printField :: Field -> Text
printField f = unwords [fname f,  "::", typeToStr (ftyp f)]
