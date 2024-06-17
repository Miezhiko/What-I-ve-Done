[![Haskell CI](https://github.com/Miezhiko/What-I-ve-Done/actions/workflows/haskell.yml/badge.svg)](https://github.com/Miezhiko/What-I-ve-Done/actions/workflows/haskell.yml)

grab info of what you've done on your previous workday
------------------------------------------------------

 - http-client and aeson-optics, nothing in addition

`conf.yml` file example:

```yml
you: "clown"
apiToken: "blbablabalababdfgreggre"
youtrackUrl: "https://youtrack.blabla.com/api/issues"
workItemsUrl: "https://youtrack.blabla.com/api/workItems"
```

```haskell
let Just summary = issue ^? key "summary" % _String
    Just issueId = issue ^? key "idReadable" % _String
    workItemsParams :: [(ByteString, Maybe ByteString)]
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
```

(Na-na, na, na, na-na, na, na)\
(Na-na, na, na, na-na, na, na)
