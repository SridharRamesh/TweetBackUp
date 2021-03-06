{-# LANGUAGE 
  OverloadedStrings, 
  TemplateHaskell, 
  RecordWildCards, 
  ScopedTypeVariables, 
  DuplicateRecordFields, 
  ViewPatterns
  #-}
module Parse (
  fileParse, 
  extractTweets
) where

import Time
import Tweet

import Data.Aeson hiding ((<?>))
import Data.Aeson.TH
import qualified Data.Attoparsec.ByteString.Lazy as LazyStringParse -- All our parsers work on ByteStrings rather than Text, because this is what Data.Aeson uses
import Data.Attoparsec.Text.Lazy
import Data.Text hiding (map) -- Strict Text because this is what Aeson uses

import Data.List
import Data.Maybe
import Data.String

-- Note: The argument to literal needs to be a STRICT ByteString, not a lazy one.
literal x = 
  do string x
     return () -- Do not expect to recover the string from this function. I've disabled this to avoid confusing myself.

monthNames :: IsString s => [s]
monthNames = ["Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"]

stringToMonth string = fmap (+1) $ elemIndex string monthNames

stringToGuaranteedMonth string = 
  fromMaybe 
    (error $ "This should be unreachable, but something went awry parsing month: " <> unpack string) 
    (stringToMonth string)

dayOfWeekNames = ["Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"]

parseSpace = literal " "
  <?> "space"
parseColon = literal ":"
  <?> "colon"
parseTimeZone = literal "+0000"
  <?> "timezone"
parseDayOfWeek = choice [literal dayOfWeek | dayOfWeek <- dayOfWeekNames]
  <?> "day of week"
parseMonth = choice [literal strictMonthName >> return lazyMonthName | (strictMonthName, lazyMonthName) <- Data.List.zip monthNames monthNames]
  <?> "month"

parseSingleDigit =
  do char <- digit
     return (read [char] :: Int)
  <?> "single digit"

parseNDigits' 0 = return 0
parseNDigits' n = 
  do prefix <- parseNDigits' (n - 1)
     lastDigit <- parseSingleDigit
     return (10 * prefix + lastDigit)
parseNDigits n = parseNDigits' n
  <?> (show n <> " digits")

parseTwoDigits = parseNDigits 2
parseFourDigits = parseNDigits 4

parseTimestamp = do
  parseDayOfWeek -- We ignore the result
  parseSpace
  monthAsString <- parseMonth
  let (month :: Int) = stringToGuaranteedMonth monthAsString
  parseSpace
  dayOfMonth <- parseTwoDigits
  parseSpace
  hour <- parseTwoDigits
  parseColon
  minute <- parseTwoDigits
  parseColon
  second <- parseTwoDigits
  parseSpace
  parseTimeZone
  parseSpace
  year <- parseFourDigits
  return Timestamp{..}
  <?> "timestamp"

textParserToJSONParser typeName textParser = withText typeName $ \text ->
  case parseOnly textParser text of
    (Left description) -> fail $ "Failure in parsing " <> typeName <> "; on text: " <> unpack text <> "; error description: " <> description
    (Right x) -> return x

instance FromJSON Timestamp where
  parseJSON = textParserToJSONParser "Timestamp" parseTimestamp

$(deriveFromJSON defaultOptions ''Tweet)
$(deriveFromJSON defaultOptions ''Entities)
$(deriveFromJSON defaultOptions ''UserMention)
$(deriveFromJSON defaultOptions{fieldLabelModifier = \x -> if x == "_type" then "type" else x} ''MediaEntry)
$(deriveFromJSON defaultOptions{rejectUnknownFields = True} ''BoxedTweet)

extractTweets bytes = case fromJSON bytes of
  Success (boxedTweets :: [BoxedTweet]) -> Success (map tweet boxedTweets)
  Error error -> Error error -- Re-exporting the error at a different type

fileParse = LazyStringParse.parse json