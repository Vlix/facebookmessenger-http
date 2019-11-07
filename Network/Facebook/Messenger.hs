{-|
Module      : Network.Facebook.Messenger
Copyright   : (c) Felix Paulusma, 2018
License     : MIT
Maintainer  : felix.paulusma@gmail.com
Stability   : semi-experimental
-}
module Network.Facebook.Messenger (
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
  ) where

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Catch (MonadThrow)

import Data.Aeson
import Data.ByteString (ByteString)
import Data.List as List (intercalate)
import Data.String (fromString)
import Data.Text as Text (Text, unpack)
import Data.Text.Encoding as TE (encodeUtf8)
import Data.Time (UTCTime)
import Data.Time.Clock.POSIX (POSIXTime)
import Network.HTTP.Client (Manager)

import qualified Web.Facebook.Messenger as FB
import Network.Facebook.Messenger.Internal
import Network.Facebook.Messenger.Types

{-

::: NOT YET INTEGRATED :::

https://developers.facebook.com/docs/messenger-platform/send-messages#attachment_reuse
https://developers.facebook.com/docs/messenger-platform/send-messages/saving-assets
https://developers.facebook.com/docs/messenger-platform/reference/attachment-upload-api/

https://developers.facebook.com/docs/messenger-platform/send-messages/broadcast-messages
https://developers.facebook.com/docs/messenger-platform/send-messages/broadcast-messages/target-broadcasts
https://developers.facebook.com/docs/messenger-platform/send-messages/broadcast-messages/estimate-reach
https://developers.facebook.com/docs/messenger-platform/reference/broadcast-api

https://developers.facebook.com/docs/messenger-platform/discovery/ads/sponsored-messages
https://developers.facebook.com/docs/messenger-platform/reference/sponsored-messages

https://developers.facebook.com/docs/messenger-platform/identity/id-matching

https://developers.facebook.com/docs/messenger-platform/built-in-nlp
-- ^ not really important?

https://developers.facebook.com/docs/messenger-platform/reference/id-matching-api

-}

-- | Send a 'SendRequest' to Facebook using an 'AccessToken'
-- and an HTTP 'Manager'
messageRequest :: (MonadIO m, MonadThrow m)
               => FB.SendRequest
               -> AccessToken
               -> Manager
               -> m (Response FB.MessageResponse FB.ErrorDetails)
messageRequest = fbPostRequest "me/messages" []

-- | Send a 'SendActionRequest' to Facebook using an 'AccessToken'
-- and an HTTP 'Manager'
senderActionRequest :: (MonadIO m, MonadThrow m)
                    => FB.SenderActionRequest
                    -> AccessToken
                    -> Manager
                    -> m (Response FB.SenderActionResponse FB.ErrorDetails)
senderActionRequest = fbPostRequest "me/messages" []


-- -------------- --
--  USER PROFILE  --
-- -------------- --

-- | Request some User Profile data from a Facebook 'FB.PSID'
-- using an 'AccessToken' and an HTTP 'Manager'
userProfileRequest :: (MonadIO m, MonadThrow m)
                   => [UserProfileType]
                   -> FB.PSID
                   -> AccessToken
                   -> Manager
                   -> m (Response FB.UserProfileResponse FB.ErrorDetails)
userProfileRequest uptypes (FB.PSID psid) =
    fbGetRequest (Text.unpack psid) [("fields", Just $ commaList uptypes)]


-- ----------------- --
--  ACCOUNT LINKING  --
-- ----------------- --

-- | Send a 'FB.PSID' request with an 'AccountLinkToken'
-- using an 'AccessToken' and an HTTP 'Manager'
psidRequest :: (MonadIO m, MonadThrow m)
            => AccountLinkToken
            -> AccessToken
            -> Manager
            -> m (Response FB.AccountLinkingResponse FB.ErrorDetails)
psidRequest (AccountLinkToken accountLinkToken) =
    let bsALToken = TE.encodeUtf8 accountLinkToken
    in  fbGetRequest "me" [("fields" , Just "recipient")
                          ,("account_linking_token", Just bsALToken)
                          ]

-- | Send an 'AccountUnlinkRequest' to Facebook using an 'AccessToken'
-- and an HTTP 'Manager'
accountUnlinkRequest :: (MonadIO m, MonadThrow m)
                     => FB.AccountUnlinkRequest
                     -> AccessToken
                     -> Manager
                     -> m (Response FB.SuccessResponse FB.ErrorDetails)
accountUnlinkRequest = fbPostRequest "me/unlink_accounts" []

-- ---------------- --
--  THREAD CONTROL  --
-- ---------------- --

-- | Send a 'PassThreadControlRequest' to Facebook using an 'AccessToken'
-- and an HTTP 'Manager'
passThreadControlRequest :: (MonadIO m, MonadThrow m)
                         => FB.PassThreadControlRequest
                         -> AccessToken
                         -> Manager
                         -> m (Response FB.ThreadControlResponse FB.ErrorDetails)
passThreadControlRequest = fbPostRequest "me/pass_thread_control" []

-- | Send a 'ThreadControlRequest' to Facebook to request a certain thread
-- using an 'AccessToken' and an HTTP 'Manager'
requestThreadControlRequest :: (MonadIO m, MonadThrow m)
                            => FB.ThreadControlRequest
                            -> AccessToken
                            -> Manager
                            -> m (Response FB.ThreadControlResponse FB.ErrorDetails)
requestThreadControlRequest = fbPostRequest "me/request_thread_control" []

-- | Send a 'ThreadControlRequest' to Facebook to take control of a certain thread
-- using an 'AccessToken' and an HTTP 'Manager'
takeThreadControlRequest :: (MonadIO m, MonadThrow m)
                         => FB.ThreadControlRequest
                         -> AccessToken
                         -> Manager
                         -> m (Response FB.ThreadControlResponse FB.ErrorDetails)
takeThreadControlRequest = fbPostRequest "me/take_thread_control" []

-- | Request a list of Secondary Receivers from Facebook
-- using an 'AccessToken' and an HTTP 'Manager'
secondaryReceiversRequest :: (MonadIO m, MonadThrow m)
                          => AccessToken
                          -> Manager
                          -> m (Response FB.SecondaryReceiverResponse FB.ErrorDetails)
secondaryReceiversRequest = fbGetRequest "me/secondary_receivers" [("fields", Just "id,name")]

-- | Request the 'AppId' of the owner of the thread using a user's 'FB.PSID',
-- an 'AccessToken' and an HTTP 'Manager'
threadOwnerRequest :: (MonadIO m, MonadThrow m)
                   => FB.PSID
                   -> AccessToken
                   -> Manager
                   -> m (Response FB.ThreadOwnerResponse FB.ErrorDetails)
threadOwnerRequest (FB.PSID psid) =
    let bsPSID = TE.encodeUtf8 psid
    in  fbGetRequest "me/thread_owner" [("recipient", Just bsPSID)]


-- ----------------- --
--  MESSENGER CODES  --
-- ----------------- --

-- | Request a Messenger Code from Facebook
-- using an 'AccessToken' and an HTTP 'Manager'
messengerCodeRequest :: (MonadIO m, MonadThrow m)
                     => FB.MessengerCodeRequest
                     -> AccessToken
                     -> Manager
                     -> m (Response FB.MessengerCodeResponse FB.ErrorDetails)
messengerCodeRequest = fbPostRequest "me/messenger_codes" []


-- ------------------- --
--  MESSENGER PROFILE  --
-- ------------------- --

-- | Request the profile settings of the Facebook App
-- using an 'AccessToken' and an HTTP 'Manager'
getMessengerProfileRequest :: (MonadIO m, MonadThrow m)
                           => [MessengerProfileType]
                           -> AccessToken
                           -> Manager
                           -> m (Response FB.GetProfileResponse FB.ErrorDetails)
getMessengerProfileRequest mptypes =
    fbGetRequest "me/messenger_profile" [("fields", Just $ commaList mptypes)]

-- | Set the profile settings of the Facebook App
-- using an 'AccessToken' and an HTTP 'Manager'
setMessengerProfileRequest :: (MonadIO m, MonadThrow m)
                           => FB.ProfileRequest
                           -> AccessToken
                           -> Manager
                           -> m (Response FB.SuccessResponse FB.ErrorDetails)
setMessengerProfileRequest = fbPostRequest "me/messenger_profile" []

-- | Request the deletion of certain settings of the Facebook app
-- using an 'AccessToken' and an HTTP 'Manager'
deleteMessengerProfileRequest :: (MonadIO m, MonadThrow m)
                              => [MessengerProfileType]
                              -> AccessToken
                              -> Manager
                              -> m (Response FB.SuccessResponse FB.ErrorDetails)
deleteMessengerProfileRequest = fbDeleteRequest "me/messenger_profile" [] . go
  where go mptypes = object [ "fields" .= (show <$> mptypes) ]

-- -------------------- --
--  MESSAGING INSIGHTS  --
-- -------------------- --

-- | Request specific metrics of the Facebook App using potential lower and/or
-- upper time bounds, an 'AccessToken' and an HTTP 'Manager'
metricRequest :: (MonadIO m, MonadThrow m)
              => [MessagingMetricsType]
              -> Maybe POSIXTime
              -> Maybe POSIXTime
              -> AccessToken
              -> Manager
              -> m (Response MetricResponse FB.ErrorDetails)
metricRequest mmtypes since until' =
    fbGetRequest "me/insights/" parameters
  where parameters = ("metric", Just metrics) : other
        other = [("since", Just $ posixToBS x) | Just x <- [since]]
             ++ [("until", Just $ posixToBS y) | Just y <- [until']]
        posixToBS = fromString . show . (truncate :: Double -> Integer) . realToFrac
        metrics = commaList mmtypes

-- | List of metrics
type MetricResponse = FB.DataResponse MetricValues

-- | Metrics from a certain period
data MetricValues = MetricValues
  { metricName :: MessagingMetricsType
  , metricPeriod :: Text
  , metricValues :: [MetricValue]
  } deriving (Eq, Show, Read)

instance FromJSON MetricValues where
  parseJSON = withObject "MetricValues" $ \o ->
      MetricValues <$> o .: "name"
                   <*> o .:? "period" .!= "day"
                   <*> o .: "values"

-- | Metric value of a certain time/period
data MetricValue = MetricValue
  { metricValue :: Value -- ^ Integer of Object, according to Facebook
  , metricEndTime :: UTCTime
  } deriving (Eq, Show, Read)

instance FromJSON MetricValue where
  parseJSON = withObject "MetricValue" $ \o ->
      MetricValue <$> o .: "value"
                  <*> o .: "end_time"


-- -------------------- --
--  MESSAGING INSIGHTS  --
-- -------------------- --

-- | Request the status of a feature of the Facebook App that's (been) in review
-- using an 'AccessToken' and an HTTP 'Manager'
featureReviewRequest :: (MonadIO m, MonadThrow m)
                     => AccessToken
                     -> Manager
                     -> m (Response FeatureReviewResponse FB.ErrorDetails)
featureReviewRequest = fbGetRequest "me/messaging_feature_review" []

-- | List of feature statuses
type FeatureReviewResponse = FB.DataResponse FeatureStatus

-- | The status of a certain feature
data FeatureStatus = FeatureStatus
    { featureName :: Text
    , featureStatus :: FeatureStatusType
    } deriving (Eq, Show, Read, Ord)

instance FromJSON FeatureStatus where
  parseJSON = withObject "FeatureStatus" $ \o ->
      FeatureStatus <$> o .: "feature"
                    <*> o .: "status"


-- ------------------ --
--  HELPER FUNCTIONS  --
-- ------------------ --

commaList :: Show a => [a] -> ByteString
commaList = fromString . List.intercalate "," . fmap show
