module Network.Facebook.Messenger.Internal (
    fbGetRequest
    , fbPostRequest
    , goHTTP
    ) where

import Data.ByteString (ByteString)
import Data.ByteString.Lazy (toStrict)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Catch (MonadThrow)

import Data.Aeson (FromJSON, ToJSON, eitherDecode', encode)
import Data.Text as Text (pack)
import Data.Text.Encoding as TE (encodeUtf8)
import Network.HTTP.Client
import Network.HTTP.Types (hContentType)

import Web.Facebook.Messenger as FB (ErrorDetails)
import Network.Facebook.Messenger.Types


fbPostRequest :: (MonadIO m, MonadThrow m, ToJSON a, FromJSON b)
              => String
              -> [(ByteString, Maybe ByteString)]
              -> a
              -> AccessToken
              -> Manager
              -> m (FBResponse b FB.ErrorDetails)
fbPostRequest url querystring a token mngr = do
    req' <- goPR url
    let req = req' { method = "POST"
                   , requestBody = RequestBodyLBS $ encode a
                   , requestHeaders = [(hContentType,"application/json")]
                   }
        request = flip setQueryString req $ accessTokenQuery token : querystring
    goHTTP request mngr

fbGetRequest :: (MonadIO m, MonadThrow m, FromJSON b)
             => String
             -> [(ByteString, Maybe ByteString)]
             -> AccessToken
             -> Manager
             -> m (FBResponse b FB.ErrorDetails)
fbGetRequest url querystring token mngr = do
    req <- goPR url
    let request = flip setQueryString req $ accessTokenQuery token : querystring
    goHTTP request mngr

goHTTP :: (MonadIO m, FromJSON a)
       => Request
       -> Manager
       -> m (FBResponse a FB.ErrorDetails)
goHTTP request mngr = do
    response <- liftIO $ httpLbs request mngr
    return $ tryError $ responseBody response
  where tryError = eitherPlus trySuccess FailureResponse
        trySuccess response firsterr =
            eitherPlus (badResponse firsterr)
                        FBResponse
                        response
        badResponse firsterr response seconderr =
            BadResponse (Text.pack firsterr)
                        (Text.pack seconderr)
                        $ toStrict response
        eitherPlus f g x = either (f x) g $ eitherDecode' x

accessTokenQuery :: AccessToken -> (ByteString, Maybe ByteString)
accessTokenQuery token = ("access_token", Just $ TE.encodeUtf8 token)

goPR :: (MonadIO m, MonadThrow m) => String -> m Request
goPR = parseRequest . mappend "https://graph.facebook.com/v2.12/"
