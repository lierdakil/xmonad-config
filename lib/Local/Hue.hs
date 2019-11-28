{-# LANGUAGE OverloadedStrings, ConstraintKinds, NoMonomorphismRestriction #-}
module Local.Hue (
  command
, Parameter(..)
, AlertValue(..)
) where

import           Data.Aeson                 ((.=))
import qualified Data.Aeson                 as JSON
import qualified Data.ByteString.Char8      as B
import qualified Data.ByteString.Lazy.Char8 as LB
import           Network.HTTP.Simple
import           Network.HTTP.Conduit (Request(..), RequestBody(..), Response(..))
import           Data.Monoid ((<>))
import           Data.Text (Text)


sendRequest :: String -> (String, B.ByteString) -> LB.ByteString -> IO String
sendRequest token (uri,method') body = do
  let api = "http://philips-hue.lan/api/" <> token
  request <- parseRequest $ api <> uri
  let req = request{method=method', requestHeaders=headers', requestBody=RequestBodyBS $ LB.toStrict body}
      headers' = [("Content-Type", "application/json")]
  resp <- httpLBS req
  return $ LB.unpack $ responseBody resp

data Parameter = ParameterAlert AlertValue
               | ParameterBrightness Int
               | ParameterColor Int
               | ParameterOn Bool
data AlertValue = AlertNone | AlertSelect

instance JSON.ToJSON AlertValue where
  toJSON AlertNone = "none"
  toJSON AlertSelect = "select"

jobj :: JSON.ToJSON a => Text -> a -> JSON.Value
jobj n v = JSON.object [ n .= JSON.toJSON v ]

instance JSON.ToJSON Parameter where
  toJSON (ParameterAlert x) = jobj "alert" x
  toJSON (ParameterBrightness x) = jobj "bri" x
  toJSON (ParameterColor x) = jobj "ct" x
  toJSON (ParameterOn x) = jobj "on" x

-- "alert" : "select",
-- "bri" : 254,
-- "ct" : 343, -- 153 - 454
-- "on" : true
command :: String -> String -> Parameter -> IO String
command token group parameter =
  sendRequest token ("/groups/" <> group <> "/action", "PUT") $ JSON.encode $ JSON.toJSON parameter
