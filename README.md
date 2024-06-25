[![Haskell CI](https://github.com/Miezhiko/What-I-ve-Done/actions/workflows/haskell.yml/badge.svg)](https://github.com/Miezhiko/What-I-ve-Done/actions/workflows/haskell.yml)

grab info of what you've done on your previous workday
------------------------------------------------------

 - http-client
 - aeson-optics

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
request <- parseRequest $ workItemsUrl mcfg
response <- httpLbs request
  { method = "GET"
  , queryString = W.renderQuery True workItemsParams
  , requestHeaders = mheaders
  } manager

case decode (responseBody response) :: Maybe [Value] of
  Nothing -> putStrLn "Error parsing response"
  Just wi -> processWorkItems wi myesterday issueId summary
```

(Na-na, na, na, na-na, na, na)\
(Na-na, na, na, na-na, na, na)
