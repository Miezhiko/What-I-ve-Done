{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}

module Main where

import           GHC.Generics

import           Optics

import           Data.Aeson
import           Data.Aeson.Optics
import           Data.ByteString         (ByteString)
import qualified Data.CaseInsensitive    as CI (mk)
import           Data.Foldable           (for_)
import           Data.List               (nub)
import           Data.Maybe              (catMaybes)
import qualified Data.Text               as T
import qualified Data.Text.Encoding      as TE
import           Data.Time
import           Data.Time.Clock.POSIX
import qualified Data.Yaml               as Yaml

import           Network.HTTP.Client
import           Network.HTTP.Client.TLS
import qualified Network.HTTP.Types      as W

data Conf
  = Conf
      { you          :: T.Text
      , apiToken     :: T.Text
      , youtrackUrl  :: String
      , workItemsUrl :: String
      }
  deriving (Eq, Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

main ∷ IO ()
main = do
  mcfg        <- Yaml.decodeFileThrow "conf.yml"
  myesterday  <- getYesterday <$> utctDay <$> getCurrentTime
  manager     <- newTlsManager

  let myou      = TE.encodeUtf8 $ you mcfg
      mytDate   = TE.encodeUtf8 $ ytDate myesterday
      mheaders  = [ (CI.mk "Authorization", "Bearer " <> (TE.encodeUtf8 $ apiToken mcfg))
                  , (CI.mk "Accept-Charset", "utf-8")]

  irequest <- parseRequest $ youtrackUrl mcfg
  iresponse <- httpLbs irequest
    { method = "GET"
    , queryString = W.renderQuery True $ params myou mytDate
    , requestHeaders = mheaders
    } manager

  case decode (responseBody iresponse) :: Maybe [Value] of
    Nothing     -> putStrLn "Error parsing response"
    Just issues ->
      let processIssue ∷ Value -> IO (Maybe [(String, String)])
          processIssue issue = do
            let Just summary = issue ^? key "summary" % _String
                Just issueId = issue ^? key "idReadable" % _String
                workItemsParams :: [(ByteString, Maybe ByteString)]
                  = [ ("query", Just $ "work author: " <> myou <> " work date: " <> mytDate)
                    , ("fields", Just "duration(presentation),text,date") ]
            request <- parseRequest $ workItemsUrl mcfg
            response <- httpLbs request
              { method = "GET"
              , queryString = W.renderQuery True workItemsParams
              , requestHeaders = mheaders
              } manager

            case decode (responseBody response) :: Maybe [Value] of
              Nothing -> pure Nothing
              Just [] -> pure Nothing
              Just wi -> pure $ Just (processWorkItems wi myesterday issueId summary)
      in do processedIssues <- traverse processIssue issues
            let pureIusses  = concat $ catMaybes processedIssues
                outIssues   = nub $ map fst pureIusses
                outSummary  = nub $ map snd pureIusses
            putStrLn "Issues:"
            for_ outIssues $ putStrLn
            putStrLn "\nWhat I've done:"
            for_ outSummary $ putStrLn

 where params ∷ ByteString -> ByteString -> [(ByteString, Maybe ByteString)]
       params y ytDateBS =
          [ ("query", Just $ "work author: " <> y <> " work date: " <> ytDateBS)
          , ("fields", Just "idReadable,summary")
          ]
  
       ytDate ∷ Day -> T.Text
       ytDate day = T.pack $ formatTime defaultTimeLocale "%Y-%m-%d" day

       getYesterday ∷ Day -> Day
       getYesterday day
          | dayOfWeek day == Monday = addDays (-3) day
          | otherwise = addDays (-1) day

processWorkItems ∷ [Value] -> Day -> T.Text -> T.Text -> [(String, String)]
processWorkItems [] _ _ _ = []
processWorkItems workItems ystd issueId summary =
  catMaybes $ map processWorkItem workItems
 where processWorkItem ∷ Value -> Maybe (String, String)
       processWorkItem workItem =
        let Just comment    = workItem ^? key "text" % _String
            Just wdate      = workItem ^? key "date" % _Integer
            wdateObject     = posixSecondsToUTCTime (fromIntegral wdate / 1000)
            wdateReadable   = formatTime defaultTimeLocale "%Y-%m-%d" wdateObject

        in if wdate >= ytTimestampWithWorkdayHours ystd
              then Just ( wdateReadable ++ ": "
                       ++ T.unpack issueId ++ " ("
                       ++ T.unpack summary ++ ")",
                          T.unpack comment )
              else Nothing

       ytTimestampWithWorkdayHours ∷ Day -> Integer
       ytTimestampWithWorkdayHours yesterday = ytTimestamp yesterday - 28800000

       ytTimestamp ∷ Day -> Integer
       ytTimestamp day = round (1000 * utcTimeToPOSIXSeconds (UTCTime day 0))
