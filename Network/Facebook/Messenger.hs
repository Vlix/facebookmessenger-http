module Network.Facebook.Messenger
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
import           Control.Monad.Catch        (MonadThrow)

import           Data.Aeson
import           Data.Aeson.Types           (typeMismatch)
import qualified Data.List                  as L
import           Data.Text                  (Text (..))
import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as TE
import           Network.HTTP.Conduit
import           Network.HTTP.Types         (hContentType)

import qualified Web.Facebook.Messenger     as FB
import           Network.Facebook.Messenger.Types


messageRequest :: (MonadIO m, MonadThrow m) => AccesToken -> FB.SendRequest -> Manager -> m (FBResponse FB.MessageResponse FB.ErrorResponse)
messageRequest accessToken request =
        fbPostRequest "https://graph.facebook.com/v2.6/me/messages?access_token="
                  accessToken
                  $ Just request

sendActionRequest :: (MonadIO m, MonadThrow m) => AccesToken -> FB.SenderActionRequest -> Manager -> m (FBResponse FB.SenderActionResponse FB.ErrorResponse)
sendActionRequest accessToken request =
        fbPostRequest "https://graph.facebook.com/v2.6/me/messages?access_token="
                  accessToken
                  $ Just request

settingsRequest :: (MonadIO m, MonadThrow m) => AccesToken -> FB.SettingsRequest -> Manager -> m (FBResponse FB.SuccessResponse FB.ErrorResponse)
settingsRequest accessToken request =
        fbPostRequest "https://graph.facebook.com/v2.6/me/thread_settings?access_token="
                  accessToken
                  $ Just request

userProfileRequest :: (MonadIO m, MonadThrow m) => AccesToken -> UserID -> [UserProfileType] -> Manager -> m (FBResponse FB.UserAPIResponse FB.ErrorResponse)
userProfileRequest accessToken userid uptypes =
        fbGetRequest ("https://graph.facebook.com/v2.6/" <> T.unpack userid
                                                         <> "?fields="
                                                         <> types
                                                         <> "&access_token=")
                  accessToken
  where
    types = L.intercalate "," $ fmap show uptypes

accountLinkingRequest :: (MonadIO m, MonadThrow m) => AccesToken -> AccountLinkToken -> Manager -> m (FBResponse FB.AccountLinkingResponse FB.ErrorResponse)
accountLinkingRequest accessToken accountLinkToken =
        fbGetRequest ("https://graph.facebook.com/v2.6/me?fields=recipient&account_linking_token="
                                                      <> T.unpack accountLinkToken
                                                      <> "&access_token=")
                  accessToken


----------------------
-- Helper Functions --
----------------------

fbPostRequest :: (MonadIO m, MonadThrow m, ToJSON a, FromJSON b) => String -> Text -> a -> Manager -> m (FBResponse b FB.ErrorResponse)
fbPostRequest s t a m = do
    req' <- goPR s t
    let req = req' { method = "POST"
                   , requestBody = RequestBodyLBS $ encode a
                   , requestHeaders = [(hContentType,"application/json")]
                   }
    goHTTP req m

fbGetRequest :: (MonadIO m, MonadThrow m, FromJSON b) => String -> Text -> Manager -> m (FBResponse b FB.ErrorResponse)
fbGetRequest s t m = do
    req <- goPR s t
    goHTTP req m

goPR :: (MonadIO m, MonadThrow m) => String -> Text -> m Request
goPR s t = parseRequest $ s <> (T.unpack t)

goHTTP :: (MonadIO m, MonadThrow m, FromJSON b) => Request -> Manager -> m (FBResponse b FB.ErrorResponse)
goHTTP req m = do
    res <- httpLbs req m
    let response = responseBody res
    case eitherDecode' response of
        Right res     -> return $ FBResponse res
        Left firsterr -> case eitherDecode' response :: Either String FB.ErrorRes of
            Right (FB.ErrorRes res) -> return $ FailureResponse res
            Left seconderr          -> return $ BadResponse (T.pack firsterr)
                                                            (T.pack seconderr)
                                                          $ (TE.decodeUtf8 . toStrict) response
