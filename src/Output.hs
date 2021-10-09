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
import qualified Data.ByteString.Lazy.UTF8 as UTF8

import Text.Blaze.Html5 as HTML
import Text.Blaze.Html5 as HTMLBase
import Text.Blaze.Html5.Attributes as HTML
import Text.Blaze.Html5.Attributes as HTMLAttributes
import Text.Blaze.Html.Renderer.Pretty

import Data.String
import Data.List as List

-- TODO: Take this from main
selfUser = User{id = "3118488162", screen_name = "RadishHarmers", name = "Sridhar Ramesh"}

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

comment x = preEscapedText ("<!-- " <> x <> " -->")

nothing = return ()

spanClass className = HTMLBase.span ! class_ (textValue className)

iconicCounter icon count = 
  do spanClass icon ! alt (textValue (icon <> " icon")) $ nothing
     text count

otherwisePreEscapedTextWithNewlines x = sequence_ $ List.intersperse br [preEscapedText line | line <- split (== '\n') x]

nbsp = preEscapedText "&nbsp;"

showScreenName screen_name = linkify ("@" <> screen_name) (textValue $ "https://twitter.com/" <> screen_name)

makeTweet :: Tweet -> Html
makeTweet tweet@Tweet{id = tweetID, ..} = p ! HTML.id (textValue tweetID) $ do
  hr
  blockquoter $ do
    img ! class_ "avi" ! src (textValue aviIcon)
    -- Note: The pretty renderer will automatically insert new lines between the following sequential lines of text, 
    -- which will become interpreted as spaces. TODO: Make this more robust to choice of renderer.
    spanClass "display-name" $ text name
    spanClass "at-name" $ preEscapedText ("@" <> screen_name)
    spanClass "timestamp" $ do
      text "·"
      text (dateToText $ date created_at)
    br
    otherwisePreEscapedTextWithNewlines full_text
    br
    br
    HTMLBase.span ! class_ (textValue "counters") $ do
      iconicCounter "Retweet" retweet_count
      iconicCounter "Like" favorite_count
  br
  text "Id: "
  linkify tweetID (textValue $ tweetToURL tweet)
  br
  text "Timestamp: "
  text $ timestampToText $ created_at
  br
  case (in_reply_to_status_id, in_reply_to_screen_name) of
    (Nothing, Nothing) -> nothing
    (Just status_id, Just screen_name) -> do 
      text "In reply to user: "
      showScreenName screen_name
      br
      text "In reply to tweet ID: "
      linkify status_id (textValue $ "https://twitter.com/" <> screen_name <> "/status/" <> status_id)
    (Just status_id, Nothing) -> do 
      text "In reply to user who no longer can be found (RIP)."
      br
      text $ "In reply to tweet ID: " <> status_id
    (Nothing, Just screen_name) -> do
      text "In reply to user: "
      showScreenName screen_name
      br
      text "In reply to tweet whose ID can't be found"
  where User{..} = selfUser

makeHtml :: Date -> [Tweet] -> Html
makeHtml date tweets = html $ do
  HTML.head $ do
    meta ! httpEquiv "Content-Type" ! content "text/html" ! charset "utf-8"
    comment "This page was generated from Tweet Back Up."
    link ! rel "stylesheet" ! href (textValue cssFile)
    HTMLBase.title $ preEscapedText $ screen_name <> ": " <> dateToText date
  body $ do
    text $ "These are all the tweets and replies that @" <> screen_name <> " made on " <> (dateToText date) <> ":"
    br
    mapM_ makeTweet tweets
  where User{..} = selfUser

makePage :: Date -> [Tweet] -> ByteString.ByteString
makePage date tweets = UTF8.fromString $ renderHtml $ makeHtml date tweets

baseURL = "../../"
cssFile = baseURL <> "styles.css"
iconFolder = baseURL <> "icons/"
likeIcon = iconFolder <> "Like.png"
retweetIcon = iconFolder <> "Retweet.png"
aviIcon = iconFolder <> "avi.jpg"
banner = iconFolder <> "Banner.jpg"

-- TODO: Linkify other people's tweets back to twitter.com.
tweetToURL Tweet{id = tweetID, ..} = baseURL <> (dateToURL $ date $ created_at) <> "#" <> tweetID