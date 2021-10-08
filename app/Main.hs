{-# LANGUAGE 
  OverloadedStrings,
  ScopedTypeVariables,
  RecordWildCards,
  ViewPatterns
  #-}
module Main where

import Parse
import Output
import Time
import Tweet

import Data.Aeson
import Data.Text.Lazy.Encoding
import qualified Data.ByteString.Lazy as ByteString
import qualified Data.ByteString.Lazy.UTF8 as UTF8
import Data.Attoparsec.ByteString.Lazy

import Control.Monad (when)
import Data.Function (on)
import Data.List (sortBy, groupBy)
import System.Directory
import System.FilePath

import Prelude hiding (writeFile)

-- This currently hardcodes its arguments.
-- Eventually, it will take them from the command line.
main :: IO ()
main = 
  do
    inputFile <- canonicalizePath "ignore/raw/FullTweetArchive.txt"
    outputDirectory <- canonicalizePath "ignore/website/"
    let selfID = "3118488162"
    main' inputFile outputDirectory selfID

main' inputFile outputDirectory selfID = do
  inputBytes <- ByteString.readFile inputFile
  case fileParse inputBytes of
    Fail unconsumedBytes _ _ -> do
      putStrLn $ "Failure parsing file " ++ inputFile ++ " as JSON"
      putStrLn "Here are the first 10 characters of the failure point:"
      putStrLn $ UTF8.toString $ ByteString.take 10 unconsumedBytes
    Done unconsumedBytes jsonObject -> do
      when (ByteString.length unconsumedBytes > 0) $ 
           putStrLn ("JSON file had leftover bit: " ++ UTF8.toString unconsumedBytes)
      case extractTweets jsonObject of
        Error errorString -> putStrLn $ "Tweets parse error: " ++ errorString
        Success tweets -> 
          let filteredTweets = filter (not . isAnyRT) tweets -- Eventually we'll allow non-self-RTs in here
              orderedTweets = sortBy (compare `on` created_at) filteredTweets
              groupedTweets = groupBy ((==) `on` (date . created_at)) orderedTweets
              datesAndHTMLBytestring = 
                [(tweetsDate, makePage tweetsDate tweets) 
                 | tweets <- groupedTweets, 
                 let tweetsDate = date $ created_at $ Prelude.head tweets]
          in do mapM_ (uncurry writePage) datesAndHTMLBytestring
                putStrLn "All done!"
  where
    writePage Date{..} page = do
      let outputPath = outputDirectory </> show year </> show month </> (show dayOfMonth <> ".html")
      -- We canonicalize the name just for more readable terminal messages in the next line
      putStrLn $ "Writing output to: " <> outputPath
      writeFile outputPath page

writeFile path content = do
  createDirectoryIfMissing True $ takeDirectory path
  ByteString.writeFile path content