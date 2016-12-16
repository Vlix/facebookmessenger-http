module Network.Facebook.Messenger.Yesod
    ( module Network.Facebook.Messenger.Types
    , messageRequest
    , senderActionRequest
    , settingsRequest
    , userProfileRequest
    , psidRequest
    , accountUnlinkRequest
    ) where

import           Data.ByteString            (ByteString)
import           Data.ByteString.Lazy       (toStrict)
import           Data.Monoid                ((<>))
import           Data.String                (fromString)
import           Control.Monad.Catch        (MonadThrow)
import           Control.Monad.IO.Class     (MonadIO)
import           Control.Monad.Reader       (MonadReader)

import           Data.Aeson
import qualified Data.List                  as L
import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as TE
import           Network.HTTP.Client.Conduit
import           Network.HTTP.Conduit       (parseRequest,setQueryString)
import           Network.HTTP.Types         (hContentType)

import qualified Web.Facebook.Messenger     as FB
import           Network.Facebook.Messenger.Types


messageRequest :: (MonadThrow m, MonadIO m, HasHttpManager env, MonadReader env m) =>
            FB.SendRequest -> AccessToken -> m (FBResponse FB.MessageResponse FB.ErrorResponse)
messageRequest sRequest accessToken = fbPostRequest accessToken "me/messages" [] sRequest


senderActionRequest :: (MonadThrow m, MonadIO m, HasHttpManager env, MonadReader env m) =>
            FB.SenderActionRequest -> AccessToken -> m (FBResponse FB.SenderActionResponse FB.ErrorResponse)
senderActionRequest saRequest accessToken = fbPostRequest accessToken "me/messages" [] saRequest


settingsRequest :: (MonadThrow m, MonadIO m, HasHttpManager env, MonadReader env m) =>
            FB.SettingsRequest -> AccessToken -> m (FBResponse FB.SuccessResponse FB.ErrorResponse)
settingsRequest setRequest accessToken = fbPostRequest accessToken "me/thread_settings" [] setRequest


userProfileRequest :: (MonadThrow m, MonadIO m, HasHttpManager env, MonadReader env m) =>
                [UserProfileType] -> UserID -> AccessToken -> m (FBResponse FB.UserAPIResponse FB.ErrorResponse)
userProfileRequest uptypes userid accessToken = fbGetRequest accessToken (T.unpack userid) [("fields", Just $ fromString types)]
  where
    types = L.intercalate "," $ fmap show uptypes


psidRequest :: (MonadThrow m, MonadIO m, HasHttpManager env, MonadReader env m) =>
            AccountLinkToken -> AccessToken -> m (FBResponse FB.AccountLinkingResponse FB.ErrorResponse)
psidRequest accountLinkToken accessToken = fbGetRequest accessToken "me" [("fields",Just "recipient"),("account_linking_token", Just $ TE.encodeUtf8 accountLinkToken)]

accountUnlinkRequest :: (MonadThrow m, MonadIO m, HasHttpManager env, MonadReader env m) =>
            FB.AccountUnlinkRequest -> AccessToken -> m (FBResponse FB.SuccessResponse FB.ErrorResponse)
accountUnlinkRequest auRequest accessToken = fbPostRequest accessToken "me/unlink_accounts" [] auRequest


----------------------
-- Helper Functions --
----------------------

accessTokenQuery :: AccessToken -> (ByteString, Maybe ByteString)
accessTokenQuery token = ("access_token", Just $ TE.encodeUtf8 token)

goPR :: (MonadThrow m, MonadIO m, HasHttpManager env, MonadReader env m) =>
    String -> m Request
goPR url = parseRequest $ "https://graph.facebook.com/v2.6/" <> url

fbPostRequest :: (MonadThrow m, MonadIO m, HasHttpManager env, MonadReader env m, ToJSON a, FromJSON b) =>
        AccessToken -> String -> [(ByteString, Maybe ByteString)] -> a -> m (FBResponse b FB.ErrorResponse)
fbPostRequest token url querystring a = do
    req' <- goPR url
    let req = req' { method = "POST"
                   , requestBody = RequestBodyLBS $ encode a
                   , requestHeaders = [(hContentType,"application/json")]
                   }
        request = flip setQueryString req $ accessTokenQuery token : querystring
    goHTTP request

fbGetRequest :: (MonadThrow m, MonadIO m, HasHttpManager env, MonadReader env m, FromJSON b) =>
        AccessToken -> String -> [(ByteString, Maybe ByteString)] -> m (FBResponse b FB.ErrorResponse)
fbGetRequest token url querystring = do
    req <- goPR url
    let request = flip setQueryString req $ accessTokenQuery token : querystring
    goHTTP request

goHTTP :: (MonadThrow m, MonadIO m, HasHttpManager env, MonadReader env m, FromJSON b) =>
        Request -> m (FBResponse b FB.ErrorResponse)
goHTTP request = do
    res <- httpLbs request
    let response = responseBody res
    case eitherDecode' response of
        Right res2    -> return $ FBResponse res2
        Left firsterr -> case eitherDecode' response :: Either String FB.ErrorRes of
            Right (FB.ErrorRes res3) -> return $ FailureResponse res3
            Left seconderr           -> return $ BadResponse (T.pack firsterr)
                                                             (T.pack seconderr)
                                                           $ toStrict response
