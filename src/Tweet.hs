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
  in_reply_to_status_id :: Maybe Text, -- Tweet numeric ID
  in_reply_to_user_id :: Maybe Text, -- User numeric ID
  in_reply_to_screen_name :: Maybe Text, -- User at name, without the @
  entities :: Entities
} deriving (Show)
tweetID Tweet{id = x} = x

data Entities = Entities {
  user_mentions :: [UserMention],
  media :: Maybe [MediaEntry]
} deriving (Show)

data UserMention = UserMention {
  id :: Text
} deriving (Show)
userMentionID UserMention{id = x} = x

data MediaEntry = MediaEntry {
  media_url :: Text,
  _type :: Text
} deriving (Show)

newtype BoxedTweet = BoxedTweet {
  tweet :: Tweet
} deriving (Show)

-- Not a Twitter archive type. My own made up type.
-- However, I have copied the field names used by Twitter for similar purposes.
data User = User {
  id :: Text, -- Numeric ID
  screen_name :: Text, -- At name, without the @
  name :: Text -- Display name
}

isAnyRT tweet = "RT @" `Text.isPrefixOf` (full_text tweet)
isSelfRT selfID tweet@Tweet{entities = Entities{user_mentions = [UserMention{id = mentionID}]}} 
  = isAnyRT tweet && mentionID == selfID
isReply tweet = case in_reply_to_status_id tweet of Nothing -> False; Just _ -> True
-- Should detect self-replies; i.e., threads. Should also perhaps detect forward on threads.
containsMultiSpace tweet = "  " `Text.isInfixOf` (full_text tweet)