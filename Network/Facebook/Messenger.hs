module Network.Facebook.Messenger
    ( module Network.Facebook.Messenger.Types
    , messageRequest
    , sendActionRequest
    , settingsRequest
    , userProfileRequest
    , psidRequest
    ) where

import           Data.Monoid                ((<>))
import           Data.ByteString            (ByteString)
import           Data.ByteString.Lazy       (toStrict)
import           Control.Monad.IO.Class     (MonadIO)
import           Control.Monad.Catch        (MonadThrow)

import           Data.Aeson
import           Data.Aeson.Types           (typeMismatch)
import qualified Data.List                  as L
import           Data.String                (fromString)
import           Data.Text                  (Text (..))
import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as TE
import           Network.HTTP.Conduit
import           Network.HTTP.Types         (hContentType)

import qualified Web.Facebook.Messenger     as FB
import           Network.Facebook.Messenger.Types


messageRequest :: (MonadIO m, MonadThrow m) => AccesToken -> FB.SendRequest -> Manager -> m (FBResponse FB.MessageResponse FB.ErrorResponse)
messageRequest accessToken sRequest = fbPostRequest accessToken "me/messages" [] sRequest

sendActionRequest :: (MonadIO m, MonadThrow m) => AccesToken -> FB.SenderActionRequest -> Manager -> m (FBResponse FB.SenderActionResponse FB.ErrorResponse)
sendActionRequest accessToken saRequest = fbPostRequest accessToken "me/messages" [] saRequest

settingsRequest :: (MonadIO m, MonadThrow m) => AccesToken -> FB.SettingsRequest -> Manager -> m (FBResponse FB.SuccessResponse FB.ErrorResponse)
settingsRequest accessToken setRequest = fbPostRequest accessToken "me/thread_settings" [] setRequest

userProfileRequest :: (MonadIO m, MonadThrow m) => AccesToken -> UserID -> [UserProfileType] -> Manager -> m (FBResponse FB.UserAPIResponse FB.ErrorResponse)
userProfileRequest accessToken userid uptypes = fbGetRequest accessToken (T.unpack userid) [("fields", Just $ fromString types)]
  where
    types = L.intercalate "," $ fmap show uptypes

psidRequest :: (MonadIO m, MonadThrow m) => AccesToken -> AccountLinkToken -> Manager -> m (FBResponse FB.AccountLinkingResponse FB.ErrorResponse)
psidRequest accessToken accountLinkToken = fbGetRequest accessToken "me" [("fields"               , Just "recipient")
                                                                         ,("account_linking_token", Just accountLinkToken)
                                                                         ]


----------------------
-- Helper Functions --
----------------------

accessTokenQuery :: AccesToken -> (ByteString, Maybe ByteString)
accessTokenQuery t = ("access_token", Just $ TE.encodeUtf8 t)

goPR :: (MonadIO m, MonadThrow m) => String -> m Request
goPR s = parseRequest $ "https://graph.facebook.com/v2.6/" <> s

fbPostRequest :: (MonadIO m, MonadThrow m, ToJSON a, FromJSON b) => AccesToken -> String -> [(ByteString, Maybe ByteString)] -> a -> Manager -> m (FBResponse b FB.ErrorResponse)
fbPostRequest t s qs a m = do
    req' <- goPR s
    let req = req' { method = "POST"
                   , requestBody = RequestBodyLBS $ encode a
                   , requestHeaders = [(hContentType,"application/json")]
                   }
        request = flip setQueryString req $ accessTokenQuery t : qs
    goHTTP request m

fbGetRequest :: (MonadIO m, MonadThrow m, FromJSON b) => AccesToken -> String -> [(ByteString, Maybe ByteString)] -> Manager -> m (FBResponse b FB.ErrorResponse)
fbGetRequest t s qs m = do
    req <- goPR s
    let request = flip setQueryString req $ accessTokenQuery t : qs
    goHTTP request m

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
