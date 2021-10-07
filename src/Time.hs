{-# LANGUAGE 
  DuplicateRecordFields,
  RecordWildCards
 #-}
module Time where

data Timestamp = Timestamp {
  year :: Int,
  month :: Int,
  dayOfMonth :: Int,
  hour :: Int,
  minute :: Int,
  second :: Int
} deriving (Show, Eq, Ord)

data Date = Date {
  year :: Int,
  month :: Int,
  dayOfMonth :: Int
} deriving (Eq, Ord)

date Timestamp{..} = Date{..}