{-# LANGUAGE TypeFamilies #-}

module Data.Recorder (
    Recorder
) where

import Database.MySQL.Simple

class Recorder a where
    type Key a
    key :: a -> Key a
    find :: ConnectInfo -> Key a -> IO (Maybe a)
    findAll :: ConnectInfo -> IO [a]
