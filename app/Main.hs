{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (
    main
    ) where

import           Data.Function
import           Data.List
import           Data.Text             hiding (filter, groupBy, map)
import           Database.MySQL.Simple
import           Options.Generic


data SqlType = SqlText | SqlInt | SqlLong | SqlBool | SqlNumeric | SqlByteArray | SqlDate | SqlTime | SqlMaybe SqlType

data HaskellType = HaskText | HaskInt | HaskLong | HaskBool | HaskNumeric | HaskByteArray | HaskDate | HaskTime | HaskMaybe HaskellType

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
haskTypeFromType (SqlMaybe t) = HaskMaybe $ haskTypeFromType t

typeToStr HaskText = "Text"
typeToStr HaskInt = "Int"
typeToStr HaskLong = "Integer"
typeToStr HaskBool = "Bool"
typeToStr HaskNumeric = ""
typeToStr HaskByteArray = "ByteArray"
typeToStr HaskDate = "Day"
typeToStr HaskTime = "UTCTime"
typeToStr (HaskMaybe t) = append "Maybe " $ typeToStr t

typeFromSql s isnull
      | isnull == "YES" = SqlMaybe $ typeFromSql s ""
      | s `elem` ["text", "varchar", "char"] = SqlText
      | s `elem` ["integer", "int", "smallint"] = SqlInt
      | s `elem` ["bigint", "long"] = SqlLong
      | s `elem` ["money", "numeric", "decimal"] = SqlNumeric
      | s `elem` ["bit", "bool", "boolean"] = SqlBool
      | s `elem` ["image", "blob", "longblob"] = SqlByteArray
      | s `elem` ["date"] = SqlDate
      | s `elem` ["datetime", "time", "timestamp"] = SqlTime

imports HaskTime = Just "Data.Time.Clock"
imports HaskDate = Just "Data.Time.Calendar"
imports HaskNumeric = Just ""
imports _ = Nothing

data Conf = Conf {
    password :: String,
    server   :: String,
    user     :: String,
    database :: String,
    package  :: String
} deriving (Generic, Show)

instance ParseRecord Conf

tableName (t, _, _, _, _, _) = t

type Result = (Text, Text, Text, Text, Text, Maybe Text)

groupByTable :: [Result] -> [[Result]]
groupByTable = groupBy ( (==) `on` tableName)

extractTables :: Conf -> IO [Table]
extractTables conf = do
    conn <- connect defaultConnectInfo { connectDatabase = database conf,
        connectPassword = password conf,
        connectUser = user conf,
        connectHost = server conf }
    let q =  "Select TABLE_NAME, COLUMN_NAME, DATA_TYPE, KEY, IS_NULLABLE, COLUMN_DEFAULT FROM COLUMNS WHERE TABLE_SCHEMA = ?"
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
    writeRecords conf records
    return ()

tableToRecord :: Table -> HaskellRecord
tableToRecord t = HaskellRecord (tname t) fields
            where
                fields = fmap columnToField (cols t)

columnToField :: Column -> Field
columnToField c = Field cname' haskTyp
    where
        cname' = cname c
        haskTyp = haskTypeFromType (ctyp c)

writeRecords :: Conf -> [HaskellRecord] -> IO ()
writeRecords conf recs = return ()
