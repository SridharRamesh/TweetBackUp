{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module Output(makePage) where

import Prelude hiding (unlines, show, id)
import qualified Prelude
import Parse
import Data.Text as Text
import qualified Data.Text as StrictText
import qualified Data.String
import Data.Maybe

import Time
import Tweet

import qualified Data.ByteString.Lazy as ByteString

import Text.Blaze.Html5 as HTML
import Text.Blaze.Html5.Attributes as HTML
import Text.Blaze.Html.Renderer.Pretty

import Data.String

show x = Data.String.fromString $ Prelude.show x

dateToText :: Date -> Text
dateToText Date{..} = (show year) <> "-" <> (show month) <> "-" <> (show dayOfMonth)

dateToURL :: Date -> Text
dateToURL Date{..} = (show year) <> "/" <> (show month) <> "/" <> (show dayOfMonth) <> ".html"

timestampToText :: Timestamp -> Text
timestampToText Timestamp{..} = 
    (show year) <> "-" <> (show month) <> "-" <> (show dayOfMonth)
    <> " at "
    <> (show hour) <> ":" <> (show minute) <> ":" <> (show second)

-- Taken from https://developer.twitter.com/en/docs/twitter-for-websites/embedded-tweets/guides/css-for-embedded-tweets
-- Along with the corresponding CSS stylesheet.
blockquoter = 
  (blockquote ! class_ (textValue "twitter-tweet") ! dataAttribute "lang" "en")
  . (p ! lang "en" ! dir "ltr")

linkify content url = (a ! href url) $ (text content)

comment x = preEscapedText ("<!-- " <> x <> " -->\n")

{-
icon altText imageURL width height = "<img class=\"icon\" src=\"" <> imageURL <> "\" alt=\"" <> altText <> "\" width=\"" <> (show width) <> "\" height=\"" <> (show height) <> "\">\n"
avi altText imageURL width height = "<img class=\"avi\" src=\"" <> imageURL <> "\" alt=\"" <> altText <> "\" width=\"" <> (show width) <> "\" height=\"" <> (show height) <> "\">\n"
-}

sourceLineBreak = text "\n"

makeTweet :: Tweet -> Html
makeTweet tweet@Tweet{id = tweetID, ..} = p ! HTML.id (textValue tweetID) $ do
  hr
  blockquoter $ do
    text "Tweet header"
    br
    text full_text
    br
    br
    text "Tweet footer"
  br
  text "Id: "
  linkify tweetID (textValue $ tweetToURL tweet)
  {-
    tweetHeader = separate $ (avi "avi" aviIcon 50 50) <> "<b>" <> "display name" <> "</b> <span class=\"display-name\" title=\"" <> (timestampToText created_at) <> "\"> " <> "@atName" <> " · " <> (dateToText $ date created_at) <> "</span>"
    tweetFooter = retweetDisplay <> " &nbsp; &nbsp; &nbsp; " <> faveDisplay
    retweetDisplay = (icon "Retweets" retweetIcon 20 20) <> (fromStrict retweet_count)
    faveDisplay = (icon "Likes" likeIcon 20 20) <> (fromStrict favorite_count)
    replyElements = case in_reply_to_status_id of
      Nothing -> []
      (Just replied_to_id) -> ["In reply to ID: " <> (fromStrict replied_to_id)]
  -}

makeHtml :: Date -> [Tweet] -> Html
makeHtml date tweets = html $ do
  HTML.head $ do
    meta ! httpEquiv "Content-Type" ! content "text/html" ! charset "utf-8"
    comment "This page was generated from Tweet Back Up."
    link ! rel "stylesheet" ! href (textValue cssFile)
  body $ do
    text "These are all the tweets and replies that I made on "
    text $ dateToText date
    text ":"
    br
    mapM_ makeTweet tweets

makePage :: Date -> [Tweet] -> ByteString.ByteString
makePage date tweets = fromString $ renderHtml $ makeHtml date tweets

baseURL = "../../"
cssFile = baseURL <> "styles.css"
iconFolder = baseURL <> "icons/"
likeIcon = iconFolder <> "Like.png"
retweetIcon = iconFolder <> "Retweet.png"
aviIcon = iconFolder <> "Avi.jpg"
banner = iconFolder <> "Banner.jpg"

-- TODO: Linkify other people's tweets back to twitter.com.
tweetToURL Tweet{id = tweetID, ..} = baseURL <> (dateToURL $ date $ created_at) <> "#" <> tweetID