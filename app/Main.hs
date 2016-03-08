{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (
    main
    ) where

import           Data.Function         (on)
import           Data.List             hiding (intercalate, unwords)
import           Data.Maybe            (mapMaybe)
import           Data.Recorder
import           Data.Text             hiding (concat, filter, groupBy, map, concatMap)
import           Data.Text.IO          (putStr, putStrLn)
import           Database.MySQL.Simple
import           Options.Generic
import           Prelude               hiding (putStr, putStrLn, unwords)

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

importStr :: HaskellType -> Maybe Text
importStr HaskTime = Just "Data.Time.Clock"
importStr HaskDate = Just "Data.Time.Calendar"
importStr HaskNumeric = Just "Data.Decimal"
importStr _ = Nothing


-- Configuration
data Conf = Conf {
    password        :: String,
    server          :: String,
    user            :: String,
    database        :: String,
    modulename      :: Text,
    lenses          :: Maybe Bool,
    lensesPackage   :: Maybe String
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
extractColumn (_, cname', dtype', iskey', isnull', def') = Column cname' (convertSqlType dtype' isnull') def' (iskey' == "PRI")

tableToRecord :: Table -> Record
tableToRecord t = Record (toTitle (tname t)) fields
            where
                fields = fmap columnToField (cols t)

columnToField :: Column -> Field
columnToField c = Field (append "_" (toCaseFold cname')) haskTyp (iskey c)
    where
        cname' = cname c
        haskTyp = haskFromSql (ctyp c)

printRecords :: Conf -> [Record] -> IO ()
printRecords conf recs = do
    putStrLn "{-# DeriveGeneric #-}\n"
    putStrLn $ unwords ["module", modulename conf, "("]
    putStrLn $ intercalate "\n," recNames
    putStrLn ") where\n"
    printImports recs
    mapM_ printRec recs
    where
        recNames = map (flip append "(..)" . hname) recs

printImports :: [Record] -> IO ()
printImports recs = do
    let flds = concatMap fields recs
        mimps = mapMaybe (importStr . ftyp) flds
        imps = map (append "import    ") mimps
    mapM_ putStrLn imps

printRec :: Record -> IO ()
printRec r = do
    putStr $ unwords [ "data", recName, "=", recName, "{\n    "]
    putStrLn $ intercalate "\n  , " fldsStr
    putStrLn $ unwords ["} deriving (Generic, Show)\n\ninstance ToJSON", recName, "\ninstance FromJSON", recName, "\n"]
    printInstance r
    where
        recName = hname r
        flds = fields r
        fldsStr = map fieldLine flds

fieldLine :: Field -> Text
fieldLine f = unwords [fname f, "::", typeToStr (ftyp f)]

printInstance :: Record -> IO ()
printInstance r = do
    putStrLn $ unwords ["instance Recorder", recName, "where"]
    putStrLn $ unwords ["    type Key", recName, "= Key (", tupleKeys ,")"]
    putStrLn "    find conn key = do"
    putStrLn $ unwords ["        x:_ <- query conn \"SELECT * FROM", lRecName, "WHERE", whereStr, "\" (", qMarks, ")"]
    putStrLn "        return x"
    putStrLn $ unwords ["    findAll conn -> query conn \"SELECT * FROM", lRecName, "\""]
    putStrLn ""
    where
        recName = hname r
        lRecName = toLower recName
        tupleKeys = intercalate ", " strKeys
        strKeys = map (typeToStr . ftyp) keys
        keys = filter fiskey $ fields r
        whereStr = intercalate " AND " $ map (\k -> append (fname k) " = ?") keys
        qMarks = intercalate ", " $ map (const "?") keys

main = do
    conf <- getRecord "Configuration"
    tables <- extractTables (conf :: Conf)
    let records = map tableToRecord tables
    printRecords conf records
