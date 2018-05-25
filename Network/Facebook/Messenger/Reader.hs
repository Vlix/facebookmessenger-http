module Network.Facebook.Messenger.Reader (
  messageRequest
  , senderActionRequest
  , userProfileRequest
  , psidRequest
  , accountUnlinkRequest
  , AccessToken
  , AccountLinkToken
  , UserProfileType (..)
  , Response (..)
  , ParseError (..)
  ) where


import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (MonadReader, asks)

import Network.HTTP.Client (HasHttpManager(..), Manager)

import qualified Web.Facebook.Messenger as FB
import qualified Network.Facebook.Messenger as NFB
import Network.Facebook.Messenger.Types


messageRequest :: (MonadThrow m, MonadIO m, HasHttpManager env, MonadReader env m)
               => FB.SendRequest
               -> AccessToken
               -> m (Response FB.MessageResponse FB.ErrorDetails)
messageRequest sRequest =
    withManager . NFB.messageRequest sRequest


senderActionRequest :: (MonadThrow m, MonadIO m, HasHttpManager env, MonadReader env m)
                    => FB.SenderActionRequest
                    -> AccessToken
                    -> m (Response FB.SenderActionResponse FB.ErrorDetails)
senderActionRequest saRequest =
    withManager . NFB.senderActionRequest saRequest


userProfileRequest :: (MonadThrow m, MonadIO m, HasHttpManager env, MonadReader env m)
                   => [UserProfileType]
                   -> FB.PSID
                   -> AccessToken
                   -> m (Response FB.UserProfileResponse FB.ErrorDetails)
userProfileRequest uptypes userid =
    withManager . NFB.userProfileRequest uptypes userid


psidRequest :: (MonadThrow m, MonadIO m, HasHttpManager env, MonadReader env m)
            => AccountLinkToken
            -> AccessToken
            -> m (Response FB.AccountLinkingResponse FB.ErrorDetails)
psidRequest accountLinkToken =
    withManager . NFB.psidRequest accountLinkToken

accountUnlinkRequest :: (MonadThrow m, MonadIO m, HasHttpManager env, MonadReader env m)
                     => FB.AccountUnlinkRequest
                     -> AccessToken
                     -> m (Response FB.SuccessResponse FB.ErrorDetails)
accountUnlinkRequest auRequest =
    withManager . NFB.accountUnlinkRequest auRequest

withManager :: (HasHttpManager env, MonadReader env m)
            => (Manager -> m a)
            -> m a
withManager f = asks getHttpManager >>= f
