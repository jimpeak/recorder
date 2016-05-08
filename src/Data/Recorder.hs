{-# LANGUAGE TypeFamilies, OverloadedStrings #-}

module Data.Recorder (
    Recorder,
    SqlType(..),
    HaskellType(..),
    Record(..),
    Column(..),
    Field(..),
    Table(..),
    haskFromSql,
    typeToStr
) where

import Database.MySQL.Simple
import Data.Text

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
    ftyp  :: HaskellType,
    fiskey :: Bool
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

typeToStr HaskText = "Text"
typeToStr HaskInt = "Int"
typeToStr HaskLong = "Integer"
typeToStr HaskBool = "Bool"
typeToStr HaskNumeric = "Decimal"
typeToStr HaskByteArray = "ByteArray"
typeToStr HaskDate = "Day"
typeToStr HaskTime = "UTCTime"
typeToStr HaskUnknown = "Unknown"
typeToStr (HaskMaybe t) = append "Maybe " $ typeToStr t

class Recorder a where
    type Key a
    key :: a -> Key a
    find :: ConnectInfo -> Key a -> IO (Maybe a)
    findAll :: ConnectInfo -> IO [a]
