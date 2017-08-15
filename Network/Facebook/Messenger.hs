module Network.Facebook.Messenger
    ( module Network.Facebook.Messenger.Types
    , messageRequest
    , senderActionRequest
    , settingsRequest
    , userProfileRequest
    , psidRequest
    , accountUnlinkRequest
    ) where

import           Data.Monoid                ((<>))
import           Data.ByteString            (ByteString)
import           Data.ByteString.Lazy       (toStrict)
import           Control.Monad.IO.Class     (MonadIO)
import           Control.Monad.Catch        (MonadThrow)

import           Data.Aeson
import qualified Data.List                  as L
import           Data.String                (fromString)
import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as TE
import           Network.HTTP.Conduit
import           Network.HTTP.Types         (hContentType)

import qualified Web.Facebook.Messenger     as FB
import           Network.Facebook.Messenger.Types


messageRequest :: (MonadIO m, MonadThrow m) => FB.SendRequest -> AccessToken -> Manager -> m (FBResponse FB.MessageResponse FB.ErrorResponse)
messageRequest sRequest accessToken = fbPostRequest accessToken "me/messages" [] sRequest

senderActionRequest :: (MonadIO m, MonadThrow m) => FB.SenderActionRequest -> AccessToken -> Manager -> m (FBResponse FB.SenderActionResponse FB.ErrorResponse)
senderActionRequest saRequest accessToken = fbPostRequest accessToken "me/messages" [] saRequest

settingsRequest :: (MonadIO m, MonadThrow m) => FB.SettingsRequest -> AccessToken -> Manager -> m (FBResponse FB.SuccessResponse FB.ErrorResponse)
settingsRequest setRequest accessToken = fbPostRequest accessToken "me/thread_settings" [] setRequest

userProfileRequest :: (MonadIO m, MonadThrow m) => [UserProfileType] -> UserID -> AccessToken -> Manager -> m (FBResponse FB.UserAPIResponse FB.ErrorResponse)
userProfileRequest uptypes userid accessToken = fbGetRequest accessToken (T.unpack userid) [("fields", Just $ fromString types)]
  where
    types = L.intercalate "," $ fmap show uptypes

psidRequest :: (MonadIO m, MonadThrow m) => AccessToken -> AccountLinkToken -> Manager -> m (FBResponse FB.AccountLinkingResponse FB.ErrorResponse)
psidRequest accountLinkToken accessToken = fbGetRequest accessToken "me" [("fields"               , Just "recipient")
                                                                         ,("account_linking_token", Just $ TE.encodeUtf8 accountLinkToken)
                                                                         ]

accountUnlinkRequest :: (MonadIO m, MonadThrow m) => FB.AccountUnlinkRequest -> AccessToken -> Manager -> m (FBResponse FB.SuccessResponse FB.ErrorResponse)
accountUnlinkRequest auRequest accessToken = fbPostRequest accessToken "me/unlink_accounts" [] auRequest


----------------------
-- Helper Functions --
----------------------

fbPostRequest :: (MonadIO m, MonadThrow m, ToJSON a, FromJSON b) => AccessToken -> String -> [(ByteString, Maybe ByteString)] -> a -> Manager -> m (FBResponse b FB.ErrorResponse)
fbPostRequest token url querystring a mngr = do
    req' <- goPR url
    let req = req' { method = "POST"
                   , requestBody = RequestBodyLBS $ encode a
                   , requestHeaders = [(hContentType,"application/json")]
                   }
        request = flip setQueryString req $ accessTokenQuery token : querystring
    goHTTP request mngr

fbGetRequest :: (MonadIO m, MonadThrow m, FromJSON b) => AccessToken -> String -> [(ByteString, Maybe ByteString)] -> Manager -> m (FBResponse b FB.ErrorResponse)
fbGetRequest token url querystring mngr = do
    req <- goPR url
    let request = flip setQueryString req $ accessTokenQuery token : querystring
    goHTTP request mngr

accessTokenQuery :: AccessToken -> (ByteString, Maybe ByteString)
accessTokenQuery token = ("access_token", Just $ TE.encodeUtf8 token)

goPR :: (MonadIO m, MonadThrow m) => String -> m Request
goPR url = parseRequest $ "https://graph.facebook.com/v2.6/" <> url

goHTTP :: (MonadIO m, MonadThrow m, FromJSON b) => Request -> Manager -> m (FBResponse b FB.ErrorResponse)
goHTTP req m = do
    res <- httpLbs req m
    let response = responseBody res
    case (eitherDecode' response :: Either String FB.ErrorRes) of
        Right (FB.ErrorRes res2) -> return $ FailureResponse res2
        Left firsterr            -> case eitherDecode' response of
                                        Right res3     -> return $ FBResponse res3
                                        Left seconderr -> return $ BadResponse (T.pack firsterr)
                                                                               (T.pack seconderr)
                                                                             $ toStrict response