{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}

module Main where

import           GHC.Generics

import           Optics

import           Control.Monad           (forM_)

import           Data.Aeson
import           Data.Aeson.Optics
import           Data.ByteString         (ByteString)
import qualified Data.CaseInsensitive    as CI (mk)
import           Data.Time
import           Data.Time.Clock.POSIX
import qualified Data.Yaml               as Yaml

import           Network.HTTP.Client
import           Network.HTTP.Client.TLS
import qualified Network.HTTP.Types      as W


import qualified Data.Text               as T
import qualified Data.Text.Encoding      as TE

data Conf
  = Conf
      { you          :: String
      , apiToken     :: String
      , youtrackUrl  :: String
      , workItemsUrl :: String
      }
  deriving (Eq, Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

toBS ∷ String -> ByteString
toBS myString = TE.encodeUtf8 (T.pack myString)

getCfg ∷ IO Conf
getCfg = Yaml.decodeFileThrow "conf.yml"

getCurrentDay ∷ IO Day
getCurrentDay = utctDay <$> getCurrentTime

getYesterday ∷ Day -> Day
getYesterday day
  | dayOfWeek day == Monday = addDays (-3) day
  | otherwise = addDays (-1) day

ytDate ∷ Day -> String
ytDate day = formatTime defaultTimeLocale "%Y-%m-%d" day

ytTimestamp ∷ Day -> Integer
ytTimestamp day = round (1000 * utcTimeToPOSIXSeconds (UTCTime day 0))

ytTimestampWithWorkdayHours ∷ Day -> Integer
ytTimestampWithWorkdayHours yesterday = ytTimestamp yesterday - 28800000

params ∷ ByteString -> ByteString -> [(ByteString, Maybe ByteString)]
params y ytDateBS =
  [ ("query", Just $ "work author: " <> y <> " work date: " <> ytDateBS)
  , ("fields", Just "idReadable,summary")
  ]

main ∷ IO ()
main = do
  mcfg        <- getCfg
  myesterday  <- getYesterday <$> getCurrentDay
  manager     <- newTlsManager

  let myous          = you mcfg
      myou           = toBS myous
      mapiToken      = apiToken mcfg
      myoutrackUrl   = youtrackUrl mcfg
      mworkItemsUrl  = workItemsUrl mcfg
      mytTimestampW  = ytTimestampWithWorkdayHours myesterday
      mytDate        = toBS (ytDate myesterday)
      mparams        = params myou mytDate
      mheaders       = [ (CI.mk "Authorization", "Bearer " <> toBS mapiToken)
                       , (CI.mk "Accept-Charset", "utf-8")]

  irequest <- parseRequest myoutrackUrl
  iresponse <- httpLbs irequest
    { method = "GET"
    , queryString = W.renderQuery True mparams
    , requestHeaders = mheaders
    } manager

  case decode (responseBody iresponse) :: Maybe [Value] of
    Nothing     -> putStrLn "Error parsing response"
    Just issues ->
      let processIssue ∷ Value -> IO ()
          processIssue issue = do
            let Just summary = issue ^? key "summary" % _String
                Just issueId = issue ^? key "idReadable" % _String

            let workItemsParams :: [(ByteString, Maybe ByteString)]
                  = [ ("query", Just $ "work author: " <> myou <> " work date: " <> mytDate)
                    , ("fields", Just "duration(presentation),text,date") ]
            request <- parseRequest mworkItemsUrl
            response <- httpLbs request
              { method = "GET"
              , queryString = W.renderQuery True workItemsParams
              , requestHeaders = mheaders
              } manager

            case decode (responseBody response) :: Maybe [Value] of
              Nothing -> putStrLn "Error parsing response"
              Just workItems ->
                let processWorkItem ∷ Value -> IO ()
                    processWorkItem workItem =
                      let Just comment    = workItem ^? key "text" % _String
                          Just wdate      = workItem ^? key "date" % _Integer
                          wdateObject     = posixSecondsToUTCTime (fromIntegral wdate / 1000)
                          wdateReadable   = formatTime defaultTimeLocale "%Y-%m-%d" wdateObject

                      in if wdate >= mytTimestampW
                            then putStrLn $ wdateReadable ++ ": "
                                          ++ T.unpack issueId ++ " ("
                                          ++ T.unpack summary ++ ") "
                                          ++ T.unpack comment
                            else pure ()
                in forM_ workItems processWorkItem
      in forM_ issues processIssue
