module Main where

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as LC
import Network.HTTP.Simple


import Lib

myToken :: BC.ByteString
myToken = "qnGwySGYzJROrWAqPeFrcCQCDVyfqJaJ"

noaaHost :: BC.ByteString
noaaHost = "www.ncdc.noaa.gov"

apiPath :: BC.ByteString
apiPath = "/cdo-web/api/v2/datasets"



buildRequestOld :: BC.ByteString -> BC.ByteString -> BC.ByteString
                    -> BC.ByteString -> Request
buildRequestOld token host method path =  setRequestMethod method 
                                        (setRequestHost host
                                            (setRequestHeader "token" [myToken]
                                            (setRequestPath path
                                                (setRequestSecure True
                                                        (setRequestPort 443 defaultRequest )))))


buildRequest :: BC.ByteString -> BC.ByteString -> BC.ByteString
             -> BC.ByteString -> Request
buildRequest token host method path  = setRequestMethod method
                                  $ setRequestHost host
                                  $ setRequestHeader "token" [token]
                                  $ setRequestPath path
                                  $ setRequestSecure True
                                  $ setRequestPort 443
                                  $ defaultRequest




request :: Request
request = buildRequest myToken noaaHost "GET" apiPath
-- now that we have built the request,
-- let us execute.

main :: IO ()
main = do
  response <- httpLBS request
  let status = getResponseStatusCode response
  if status == 200
    then do
         print "saving request to file"
         let jsonBody = getResponseBody response
         L.writeFile "data.json" jsonBody
    else print "request failed with error"