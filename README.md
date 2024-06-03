[![Haskell CI](https://github.com/Miezhiko/What-I-ve-Done/actions/workflows/haskell.yml/badge.svg)](https://github.com/Miezhiko/What-I-ve-Done/actions/workflows/haskell.yml)

grab info of what you've done on your previous workday
------------------------------------------------------

(Na-na, na, na, na-na, na, na)
(Na-na, na, na, na-na, na, na)

`conf.yml` file example:

```yml
you: "clown"
apiToken: "blbablabalababdfgreggre"
youtrackUrl: "https://youtrack.blabla.com/api/issues"
workItemsUrl: "https://youtrack.blabla.com/api/workItems"
```

```haskell
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
```
