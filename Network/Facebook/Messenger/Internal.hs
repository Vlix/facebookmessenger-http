module Network.Facebook.Messenger.Internal (
    fbGetRequest
    , fbPostRequest
    , fbDeleteRequest
    ) where

import Data.ByteString (ByteString)
import Data.ByteString.Lazy (toStrict)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Catch (MonadThrow)

import Data.Aeson (FromJSON, ToJSON, eitherDecode', encode)
import Data.Text as Text (pack)
import Data.Text.Encoding as TE (encodeUtf8)
import qualified Network.HTTP.Client as HTTP
import Network.HTTP.Types (hContentType)

import Web.Facebook.Messenger as FB (erError, ErrorDetails)
import Network.Facebook.Messenger.Types


fbPostRequest :: (MonadIO m, MonadThrow m, ToJSON a, FromJSON b)
              => String
              -> [(ByteString, Maybe ByteString)]
              -> a
              -> AccessToken
              -> HTTP.Manager
              -> m (Response b FB.ErrorDetails)
fbPostRequest url querystring a = mkRequest setReq url querystring
  where setReq req = req { HTTP.method = "POST"
                         , HTTP.requestBody = HTTP.RequestBodyLBS $ encode a
                         , HTTP.requestHeaders = [(hContentType,"application/json")]
                         }

fbGetRequest :: (MonadIO m, MonadThrow m, FromJSON b)
             => String
             -> [(ByteString, Maybe ByteString)]
             -> AccessToken
             -> HTTP.Manager
             -> m (Response b FB.ErrorDetails)
fbGetRequest = mkRequest id

fbDeleteRequest :: (MonadIO m, MonadThrow m, ToJSON a, FromJSON b)
                => String
                -> [(ByteString, Maybe ByteString)]
                -> a
                -> AccessToken
                -> HTTP.Manager
                -> m (Response b FB.ErrorDetails)
fbDeleteRequest url querystring a = mkRequest setReq url querystring
  where setReq req = req { HTTP.method = "DELETE"
                         , HTTP.requestBody = HTTP.RequestBodyLBS $ encode a
                         , HTTP.requestHeaders = [(hContentType,"application/json")]
                         }

mkRequest :: (MonadIO m, MonadThrow m, FromJSON b)
          => (HTTP.Request -> HTTP.Request)
          -> String
          -> [(ByteString, Maybe ByteString)]
          -> AccessToken
          -> HTTP.Manager
          -> m (Response b FB.ErrorDetails)
mkRequest f url querystring token mngr = do
    req <- goPR url
    let newReq = f req
        newQueryString = accessTokenQuery token : querystring
        request = HTTP.setQueryString newQueryString newReq
    goHTTP request mngr

goHTTP :: (MonadIO m, FromJSON a)
       => HTTP.Request
       -> HTTP.Manager
       -> m (Response a FB.ErrorDetails)
goHTTP request mngr = do
    response <- liftIO $ HTTP.httpLbs request mngr
    let status = HTTP.responseStatus response
        body = HTTP.responseBody response
    return $ tryError status body
  where tryError status res =
            either trySuccess (FailureResponse status . FB.erError) eDecoded
          where eDecoded :: FromJSON a => Either String a
                eDecoded = eitherDecode' res
                trySuccess errorFail =
                    either (badResponse errorFail) Response eDecoded
                badResponse errorFail successFail =
                    BadResponse status $ ParseError
                        (Text.pack successFail)
                        (Text.pack errorFail)
                        $ toStrict res

accessTokenQuery :: AccessToken -> (ByteString, Maybe ByteString)
accessTokenQuery (AccessToken token) =
    ("access_token", Just $ TE.encodeUtf8 token)

goPR :: (MonadIO m, MonadThrow m) => String -> m HTTP.Request
goPR = HTTP.parseRequest . mappend "https://graph.facebook.com/v2.12/"
