{-# LANGUAGE 
  OverloadedStrings,
  DuplicateRecordFields,
  RecordWildCards 
  #-}
module Tweet where

import Data.Text as Text
import Time

-- All field names in the following exactly match those used in Twitter 
-- JSON archives.
--
-- This is sometimes a bother for the fields named `id`, so we provide 
-- alternatively named accessors.
data Tweet = Tweet {
  id :: Text,
  full_text  :: Text,
  created_at :: Timestamp,
  favorite_count :: Text,
  retweet_count :: Text,
  in_reply_to_status_id :: Maybe Text,
  in_reply_to_user_id :: Maybe Text,
  in_reply_to_screen_name :: Maybe Text,
  entities :: Entities
} deriving (Show)
tweetID Tweet{id = x} = x

data Entities = Entities {
  user_mentions :: [UserMention]
} deriving (Show)

data UserMention = UserMention {
  id :: Text
} deriving (Show)
userMentionID UserMention{id = x} = x

isAnyRT tweet = "RT @" `Text.isPrefixOf` (full_text tweet)
isSelfRT selfID tweet@Tweet{entities = Entities{user_mentions = [UserMention{id = mentionID}]}} 
  = isAnyRT tweet && mentionID == selfID
isReply tweet = case in_reply_to_status_id tweet of Nothing -> False; Just _ -> True
-- Should detect self-replies; i.e., threads. Should also perhaps detect forward on threads.
containsMultiSpace tweet = "  " `Text.isInfixOf` (full_text tweet)