module Network.Facebook.Messenger.Yesod
    ( module Network.Facebook.Messenger.Types
    , messageRequest
    , sendActionRequest
    , settingsRequest
    , userProfileRequest
    , psidRequest
    ) where

import           Data.ByteString            (ByteString)
import           Data.ByteString.Lazy       (toStrict)
import           Data.Monoid                ((<>))
import           Data.String                (fromString)
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
import           Network.HTTP.Conduit       (parseRequest,setQueryString)
import           Network.HTTP.Types         (hContentType)

import qualified Web.Facebook.Messenger     as FB
import           Network.Facebook.Messenger.Types


messageRequest :: (MonadThrow m, MonadIO m, HasHttpManager env, MonadReader env m)
                    => AccesToken -> FB.SendRequest -> m (FBResponse FB.MessageResponse FB.ErrorResponse)
messageRequest accessToken sRequest = fbPostRequest accessToken "me/messages" [] sRequest

sendActionRequest :: (MonadThrow m, MonadIO m, HasHttpManager env, MonadReader env m)
                    => AccesToken -> FB.SenderActionRequest -> m (FBResponse FB.SenderActionResponse FB.ErrorResponse)
sendActionRequest accessToken saRequest = fbPostRequest accessToken "me/messages" [] saRequest

settingsRequest :: (MonadThrow m, MonadIO m, HasHttpManager env, MonadReader env m)
                    => AccesToken -> FB.SettingsRequest -> m (FBResponse FB.SuccessResponse FB.ErrorResponse)
settingsRequest accessToken setRequest = fbPostRequest accessToken "me/thread_settings" [] setRequest

userProfileRequest :: (MonadThrow m, MonadIO m, HasHttpManager env, MonadReader env m)
                    => AccesToken -> UserID -> [UserProfileType] -> m (FBResponse FB.UserAPIResponse FB.ErrorResponse)
userProfileRequest accessToken userid uptypes = fbGetRequest accessToken (T.unpack userid) [("fields", Just $ fromString types)]
  where
    types = L.intercalate "," $ fmap show uptypes

psidRequest :: (MonadThrow m, MonadIO m, HasHttpManager env, MonadReader env m)
                    => AccesToken -> AccountLinkToken -> m (FBResponse FB.UserAPIResponse FB.ErrorResponse)
psidRequest accessToken accountLinkToken = fbGetRequest accessToken "me" [("fields",Just "recipient"),("account_linking_token", Just accountLinkToken)]
                                                      


----------------------
-- Helper Functions --
----------------------

accessTokenQuery :: AccesToken -> (ByteString, Maybe ByteString)
accessTokenQuery t = ("access_token", Just $ TE.encodeUtf8 t)

goPR :: (MonadThrow m, MonadIO m, HasHttpManager env, MonadReader env m)
            => String -> m Request
goPR s = parseRequest $ "https://graph.facebook.com/v2.6/" <> s

fbPostRequest :: (MonadThrow m, MonadIO m, HasHttpManager env, MonadReader env m, ToJSON a, FromJSON b)
                    => AccesToken -> String -> [(ByteString, Maybe ByteString)] -> a -> m (FBResponse b FB.ErrorResponse)
fbPostRequest t s qs a = do
    req' <- goPR s
    let req = req' { method = "POST"
                   , requestBody = RequestBodyLBS $ encode a
                   , requestHeaders = [(hContentType,"application/json")]
                   }
        request = flip setQueryString req $ accessTokenQuery t : qs
    goHTTP request

fbGetRequest :: (MonadThrow m, MonadIO m, HasHttpManager env, MonadReader env m, FromJSON b)
                    => AccesToken -> String -> [(ByteString, Maybe ByteString)] -> m (FBResponse b FB.ErrorResponse)
fbGetRequest t s qs = do
    req <- goPR s
    let request = flip setQueryString req $ accessTokenQuery t : qs
    goHTTP request

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
