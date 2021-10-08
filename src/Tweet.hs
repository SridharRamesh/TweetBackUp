{-# LANGUAGE 
  OverloadedStrings,
  DuplicateRecordFields,
  RecordWildCards 
  #-}
module Tweet where

import Data.Text as Text
import Time

import Data.String

-- All field names in the following exactly match those used in Twitter 
-- JSON archives.
data Tweet = Tweet {
  id :: Text,
  full_text  :: Text,
  created_at :: Timestamp,
  favorite_count :: Text,
  retweet_count :: Text,
  in_reply_to_status_id :: Maybe Text,
  in_reply_to_user_id :: Maybe Text,
  in_reply_to_screen_name :: Maybe Text,
  entities :: Maybe Entities
} deriving (Show)

data Entities = Entities {
  user_mentions :: [UserMention]
} deriving (Show)

data UserMention = UserMention {
  id :: Text
} deriving (Show)

isAnyRT tweet = "RT @" `Text.isPrefixOf` (full_text tweet)
isSelfRT selfID tweet@Tweet{entities = Just (Entities{user_mentions = [UserMention{id = mentionID}]})} 
  = isAnyRT tweet && mentionID == selfID
isReply tweet = case in_reply_to_status_id tweet of Nothing -> False; Just _ -> True
-- Should detect self-replies; i.e., threads. Should also perhaps detect forward on threads.
containsMultiSpace tweet = "  " `Text.isInfixOf` (full_text tweet)