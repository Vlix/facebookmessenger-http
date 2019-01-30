{-|
Module      : Network.Facebook.Messenger.Reader
Copyright   : (c) Felix Paulusma, 2018
License     : MIT
Maintainer  : felix.paulusma@gmail.com
Stability   : semi-experimental
-}
module Network.Facebook.Messenger.Reader (
  -- * Types
  AccessToken (..)
  , Response (..)
  , ParseError (..)
  -- * Requests
  , messageRequest
  , senderActionRequest
  -- ** User Profile
  , userProfileRequest
  , UserProfileType (..)
  -- ** Account Linking
  , psidRequest
  , accountUnlinkRequest
  , AccountLinkToken (..)

{-
  -- ** Thread Control
  , passThreadControlRequest
  , requestThreadControlRequest
  , takeThreadControlRequest
  , secondaryReceiversRequest
  , threadOwnerRequest
  -- ** Messenger Code
  , messengerCodeRequest
  -- ** Messenger Profile
  , getMessengerProfileRequest
  , setMessengerProfileRequest
  , deleteMessengerProfileRequest
  , MessengerProfileType(..)
  -- ** Messaging Insights
  , metricRequest
  , MessagingMetricsType(..)
  , MetricResponse
  , MetricValues (..)
  , MetricValue (..)
  -- ** Messaging Feature Review
  , featureReviewRequest
  , FeatureReviewResponse
  , FeatureStatus (..)
  , FeatureStatusType (..)
-}

  ) where


import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (MonadReader, asks)

import Network.HTTP.Client (HasHttpManager(..), Manager)

import qualified Web.Facebook.Messenger as FB
import qualified Network.Facebook.Messenger as NFB
import Network.Facebook.Messenger.Types

-- | The same as 'NFB.messageRequest', but gets the HTTP 'Manager' from the environment.
messageRequest :: (MonadThrow m, MonadIO m, HasHttpManager env, MonadReader env m)
               => FB.SendRequest
               -> AccessToken
               -> m (Response FB.MessageResponse FB.ErrorDetails)
messageRequest sRequest =
    withManager . NFB.messageRequest sRequest

-- | The same as 'NFB.senderActionRequest', but gets the HTTP 'Manager' from the environment.
senderActionRequest :: (MonadThrow m, MonadIO m, HasHttpManager env, MonadReader env m)
                    => FB.SenderActionRequest
                    -> AccessToken
                    -> m (Response FB.SenderActionResponse FB.ErrorDetails)
senderActionRequest saRequest =
    withManager . NFB.senderActionRequest saRequest

-- | The same as 'NFB.userProfileRequest', but gets the HTTP 'Manager' from the environment.
userProfileRequest :: (MonadThrow m, MonadIO m, HasHttpManager env, MonadReader env m)
                   => [UserProfileType]
                   -> FB.PSID
                   -> AccessToken
                   -> m (Response FB.UserProfileResponse FB.ErrorDetails)
userProfileRequest uptypes userid =
    withManager . NFB.userProfileRequest uptypes userid

-- | The same as 'NFB.psidRequest', but gets the HTTP 'Manager' from the environment.
psidRequest :: (MonadThrow m, MonadIO m, HasHttpManager env, MonadReader env m)
            => AccountLinkToken
            -> AccessToken
            -> m (Response FB.AccountLinkingResponse FB.ErrorDetails)
psidRequest accountLinkToken =
    withManager . NFB.psidRequest accountLinkToken

-- | The same as 'NFB.accountUnlinkRequest', but gets the HTTP 'Manager' from the environment.
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
