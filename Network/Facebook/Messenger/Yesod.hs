module Network.Facebook.Messenger.Yesod
    ( module Network.Facebook.Messenger.Types
    , messageRequest
    , sendActionRequest
    , settingsRequest
    , userProfileRequest
    , accountLinkingRequest
    ) where

import           Data.Monoid                ((<>))
import           Data.ByteString.Lazy       (toStrict)
import           Control.Monad.IO.Class     (MonadIO)
import           Control.Monad.Reader       (MonadReader)
import           Control.Monad.Catch        (MonadThrow)

import           Data.Aeson
import           Data.Aeson.Types           (typeMismatch)
import qualified Data.List                  as L
import           Data.Text                  (Text (..))
import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as TE
import           Network.HTTP.Client.Conduit
import           Network.HTTP.Types         (hContentType)

import qualified Web.Facebook.Messenger     as FB
import           Network.Facebook.Messenger.Types


messageRequest :: (MonadThrow m, MonadIO m, HasHttpManager env, MonadReader env m)
                    => AccesToken -> FB.SendRequest -> m (FBResponse FB.MessageResponse FB.ErrorResponse)
messageRequest accessToken request =
        fbPostRequest "https://graph.facebook.com/v2.6/me/messages?access_token="
                  accessToken
                  $ Just request

sendActionRequest :: (MonadThrow m, MonadIO m, HasHttpManager env, MonadReader env m)
                    => AccesToken -> FB.SenderActionRequest -> m (FBResponse FB.SenderActionResponse FB.ErrorResponse)
sendActionRequest accessToken request =
        fbPostRequest "https://graph.facebook.com/v2.6/me/messages?access_token="
                  accessToken
                  $ Just request

settingsRequest :: (MonadThrow m, MonadIO m, HasHttpManager env, MonadReader env m)
                    => AccesToken -> FB.SettingsRequest -> m (FBResponse FB.SuccessResponse FB.ErrorResponse)
settingsRequest accessToken request =
        fbPostRequest "https://graph.facebook.com/v2.6/me/thread_settings?access_token="
                  accessToken
                  $ Just request

userProfileRequest :: (MonadThrow m, MonadIO m, HasHttpManager env, MonadReader env m)
                    => AccesToken -> UserID -> [UserProfileType] -> m (FBResponse FB.UserAPIResponse FB.ErrorResponse)
userProfileRequest accessToken userid uptypes =
        fbGetRequest ("https://graph.facebook.com/v2.6/" <> T.unpack userid
                                                         <> "?fields="
                                                         <> types
                                                         <> "&access_token=")
                  accessToken
  where
    types = L.intercalate "," $ fmap show uptypes

accountLinkingRequest :: (MonadThrow m, MonadIO m, HasHttpManager env, MonadReader env m)
                    => AccesToken -> AccountLinkToken -> m (FBResponse FB.UserAPIResponse FB.ErrorResponse)
accountLinkingRequest accessToken accountLinkToken =
        fbGetRequest ("https://graph.facebook.com/v2.6/me?fields=recipient&account_linking_token="
                                                      <> T.unpack accountLinkToken
                                                      <> "&access_token=")
                  accessToken


----------------------
-- Helper Functions --
----------------------

fbPostRequest :: (MonadThrow m, MonadIO m, HasHttpManager env, MonadReader env m, ToJSON a, FromJSON b)
                    => String -> Text -> a -> m (FBResponse b FB.ErrorResponse)
fbPostRequest s t a = do
    req' <- goPR s t
    let req = req' { method = "POST"
                   , requestBody = RequestBodyLBS $ encode a
                   , requestHeaders = [(hContentType,"application/json")]
                   }
    goHTTP req

fbGetRequest :: (MonadThrow m, MonadIO m, HasHttpManager env, MonadReader env m, FromJSON b)
                    => String -> Text -> m (FBResponse b FB.ErrorResponse)
fbGetRequest s t = do
    req <- goPR s t
    goHTTP req

goPR :: (MonadThrow m, MonadIO m, HasHttpManager env, MonadReader env m)
            => String -> Text -> m Request
goPR s t = parseRequest $ s <> (T.unpack t)

goHTTP :: (MonadThrow m, MonadIO m, HasHttpManager env, MonadReader env m, FromJSON b)
            => Request -> m (FBResponse b FB.ErrorResponse)
goHTTP y = do
    res <- httpLbs y
    let response = responseBody res
    case eitherDecode' response of
        Right res     -> return $ FBResponse res
        Left firsterr -> case eitherDecode' response :: Either String FB.ErrorRes of
            Right (FB.ErrorRes res) -> return $ FailureResponse res
            Left seconderr          -> return $ BadResponse (T.pack firsterr)
                                                            (T.pack seconderr)
                                                          $ (TE.decodeUtf8 . toStrict) response
