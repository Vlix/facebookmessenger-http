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
  , messageRequest'
  , senderActionRequest
  , senderActionRequest'
  -- ** User Profile
  , userProfileRequest
  , userProfileRequest'
  , UserProfileType (..)
  -- ** Account Linking
  , psidRequest
  , psidRequest'
  , accountUnlinkRequest
  , accountUnlinkRequest'
  , AccountLinkToken (..)
  -- ** Thread Control
  , passThreadControlRequest
  , passThreadControlRequest'
  , requestThreadControlRequest
  , requestThreadControlRequest'
  , takeThreadControlRequest
  , takeThreadControlRequest'
  , secondaryReceiversRequest
  , secondaryReceiversRequest'
  , threadOwnerRequest
  , threadOwnerRequest'
  -- ** Messenger Code
  , messengerCodeRequest
  , messengerCodeRequest'
  -- ** Messenger Profile
  , getMessengerProfileRequest
  , getMessengerProfileRequest'
  , setMessengerProfileRequest
  , setMessengerProfileRequest'
  , deleteMessengerProfileRequest
  , deleteMessengerProfileRequest'
  , MessengerProfileType(..)
  -- ** Messaging Insights
  , metricRequest
  , metricRequest'
  , MessagingMetricsType(..)
  , MetricResponse
  , MetricValues (..)
  , MetricValue (..)
  -- ** Messaging Feature Review
  , featureReviewRequest
  , featureReviewRequest'
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
import Data.Version (Version(..))
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

version3'2 :: Version
version3'2 = Version [3,2] []

-- | Send a 'SendRequest' to Facebook using an 'AccessToken'
-- and an HTTP 'Manager'
messageRequest :: (MonadIO m, MonadThrow m)
               => FB.SendRequest
               -> AccessToken
               -> Manager
               -> m (Response FB.MessageResponse FB.ErrorDetails)
messageRequest = flip messageRequest' version3'2

-- | Same as 'messageRequest', but you can set the version of the API.
messageRequest' :: (MonadIO m, MonadThrow m)
                => FB.SendRequest
                -> Version
                -> AccessToken
                -> Manager
                -> m (Response FB.MessageResponse FB.ErrorDetails)
messageRequest' req version = fbPostRequest version "me/messages" [] req

-- | Send a 'SendActionRequest' to Facebook using an 'AccessToken'
-- and an HTTP 'Manager'
senderActionRequest :: (MonadIO m, MonadThrow m)
                    => FB.SenderActionRequest
                    -> AccessToken
                    -> Manager
                    -> m (Response FB.SenderActionResponse FB.ErrorDetails)
senderActionRequest = flip senderActionRequest' version3'2

-- | Same as 'senderActionRequest', but you can set the version of the API.
senderActionRequest' :: (MonadIO m, MonadThrow m)
                     => FB.SenderActionRequest
                     -> Version
                     -> AccessToken
                     -> Manager
                     -> m (Response FB.SenderActionResponse FB.ErrorDetails)
senderActionRequest' req version = fbPostRequest version "me/messages" [] req


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
userProfileRequest uptypes psid =
    userProfileRequest' uptypes psid version3'2

-- | Same as 'userProfileRequest', but you can set the version of the API.
userProfileRequest' :: (MonadIO m, MonadThrow m)
                    => [UserProfileType]
                    -> FB.PSID
                    -> Version
                    -> AccessToken
                    -> Manager
                    -> m (Response FB.UserProfileResponse FB.ErrorDetails)
userProfileRequest' uptypes (FB.PSID psid) version =
    fbGetRequest version (Text.unpack psid) [("fields", Just $ commaList uptypes)]


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
psidRequest = flip psidRequest' version3'2

-- | Same as 'psidRequest', but you can set the version of the API.
psidRequest' :: (MonadIO m, MonadThrow m)
             => AccountLinkToken
             -> Version
             -> AccessToken
             -> Manager
             -> m (Response FB.AccountLinkingResponse FB.ErrorDetails)
psidRequest' (AccountLinkToken accountLinkToken) version =
    let bsALToken = TE.encodeUtf8 accountLinkToken
    in  fbGetRequest
            version
            "me"
            [("fields" , Just "recipient")
            ,("account_linking_token", Just bsALToken)
            ]

-- | Send an 'AccountUnlinkRequest' to Facebook using an 'AccessToken'
-- and an HTTP 'Manager'
accountUnlinkRequest :: (MonadIO m, MonadThrow m)
                     => FB.AccountUnlinkRequest
                     -> AccessToken
                     -> Manager
                     -> m (Response FB.SuccessResponse FB.ErrorDetails)
accountUnlinkRequest = flip accountUnlinkRequest' version3'2

-- | Same as 'accountUnlinkRequest', but you can set the version of the API.
accountUnlinkRequest' :: (MonadIO m, MonadThrow m)
                      => FB.AccountUnlinkRequest
                      -> Version
                      -> AccessToken
                      -> Manager
                      -> m (Response FB.SuccessResponse FB.ErrorDetails)
accountUnlinkRequest' req version =
    fbPostRequest version "me/unlink_accounts" [] req


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
passThreadControlRequest = flip passThreadControlRequest' version3'2

-- | Same as 'passThreadControlRequest', but you can set the version of the API.
passThreadControlRequest' :: (MonadIO m, MonadThrow m)
                          => FB.PassThreadControlRequest
                          -> Version
                          -> AccessToken
                          -> Manager
                          -> m (Response FB.ThreadControlResponse FB.ErrorDetails)
passThreadControlRequest' req version =
    fbPostRequest version "me/pass_thread_control" [] req

-- | Send a 'ThreadControlRequest' to Facebook to request a certain thread
-- using an 'AccessToken' and an HTTP 'Manager'
requestThreadControlRequest :: (MonadIO m, MonadThrow m)
                            => FB.ThreadControlRequest
                            -> AccessToken
                            -> Manager
                            -> m (Response FB.ThreadControlResponse FB.ErrorDetails)
requestThreadControlRequest = flip requestThreadControlRequest' version3'2

-- | Same as 'requestThreadControlRequest', but you can set the version of the API.
requestThreadControlRequest' :: (MonadIO m, MonadThrow m)
                             => FB.ThreadControlRequest
                             -> Version
                             -> AccessToken
                             -> Manager
                             -> m (Response FB.ThreadControlResponse FB.ErrorDetails)
requestThreadControlRequest' req version =
    fbPostRequest version "me/request_thread_control" [] req

-- | Send a 'ThreadControlRequest' to Facebook to take control of a certain thread
-- using an 'AccessToken' and an HTTP 'Manager'
takeThreadControlRequest :: (MonadIO m, MonadThrow m)
                         => FB.ThreadControlRequest
                         -> AccessToken
                         -> Manager
                         -> m (Response FB.ThreadControlResponse FB.ErrorDetails)
takeThreadControlRequest = flip takeThreadControlRequest' version3'2

-- | Same as 'takeThreadControlRequest', but you can set the version of the API.
takeThreadControlRequest' :: (MonadIO m, MonadThrow m)
                          => FB.ThreadControlRequest
                          -> Version
                          -> AccessToken
                          -> Manager
                          -> m (Response FB.ThreadControlResponse FB.ErrorDetails)
takeThreadControlRequest' req version =
    fbPostRequest version "me/take_thread_control" [] req

-- | Request a list of Secondary Receivers from Facebook
-- using an 'AccessToken' and an HTTP 'Manager'
secondaryReceiversRequest :: (MonadIO m, MonadThrow m)
                          => AccessToken
                          -> Manager
                          -> m (Response FB.SecondaryReceiverResponse FB.ErrorDetails)
secondaryReceiversRequest = secondaryReceiversRequest' version3'2

-- | Same as 'secondaryReceiversRequest', but you can set the version of the API.
secondaryReceiversRequest' :: (MonadIO m, MonadThrow m)
                           => Version
                           -> AccessToken
                           -> Manager
                           -> m (Response FB.SecondaryReceiverResponse FB.ErrorDetails)
secondaryReceiversRequest' version =
    fbGetRequest version "me/secondary_receivers" [("fields", Just "id,name")]

-- | Request the 'AppId' of the owner of the thread using a user's 'FB.PSID',
-- an 'AccessToken' and an HTTP 'Manager'
threadOwnerRequest :: (MonadIO m, MonadThrow m)
                   => FB.PSID
                   -> AccessToken
                   -> Manager
                   -> m (Response FB.ThreadOwnerResponse FB.ErrorDetails)
threadOwnerRequest = flip threadOwnerRequest' version3'2

-- | Same as 'threadOwnerRequest', but you can set the version of the API.
threadOwnerRequest' :: (MonadIO m, MonadThrow m)
                    => FB.PSID
                    -> Version
                    -> AccessToken
                    -> Manager
                    -> m (Response FB.ThreadOwnerResponse FB.ErrorDetails)
threadOwnerRequest' (FB.PSID psid) version =
    let bsPSID = TE.encodeUtf8 psid
    in  fbGetRequest version "me/thread_owner" [("recipient", Just bsPSID)]


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
messengerCodeRequest = flip messengerCodeRequest' version3'2

-- | Same as 'messengerCodeRequest', but you can set the version of the API.
messengerCodeRequest' :: (MonadIO m, MonadThrow m)
                      => FB.MessengerCodeRequest
                      -> Version
                      -> AccessToken
                      -> Manager
                      -> m (Response FB.MessengerCodeResponse FB.ErrorDetails)
messengerCodeRequest' req version =
    fbPostRequest version "me/messenger_codes" [] req


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
getMessengerProfileRequest = flip getMessengerProfileRequest' version3'2

-- | Same as 'getMessengerProfileRequest', but you can set the version of the API.
getMessengerProfileRequest' :: (MonadIO m, MonadThrow m)
                            => [MessengerProfileType]
                            -> Version
                            -> AccessToken
                            -> Manager
                            -> m (Response FB.GetProfileResponse FB.ErrorDetails)
getMessengerProfileRequest' mptypes version =
    fbGetRequest version "me/messenger_profile" [("fields", Just $ commaList mptypes)]

-- | Set the profile settings of the Facebook App
-- using an 'AccessToken' and an HTTP 'Manager'
setMessengerProfileRequest :: (MonadIO m, MonadThrow m)
                           => FB.ProfileRequest
                           -> AccessToken
                           -> Manager
                           -> m (Response FB.SuccessResponse FB.ErrorDetails)
setMessengerProfileRequest = flip setMessengerProfileRequest' version3'2

-- | Same as 'setMessengerProfileRequest', but you can set the version of the API.
setMessengerProfileRequest' :: (MonadIO m, MonadThrow m)
                            => FB.ProfileRequest
                            -> Version
                            -> AccessToken
                            -> Manager
                            -> m (Response FB.SuccessResponse FB.ErrorDetails)
setMessengerProfileRequest' req version =
    fbPostRequest version "me/messenger_profile" [] req

-- | Request the deletion of certain settings of the Facebook app
-- using an 'AccessToken' and an HTTP 'Manager'
deleteMessengerProfileRequest :: (MonadIO m, MonadThrow m)
                              => [MessengerProfileType]
                              -> AccessToken
                              -> Manager
                              -> m (Response FB.SuccessResponse FB.ErrorDetails)
deleteMessengerProfileRequest = flip deleteMessengerProfileRequest' version3'2

-- | Same as 'deleteMessengerProfileRequest', but you can set the version of the API.
deleteMessengerProfileRequest' :: (MonadIO m, MonadThrow m)
                               => [MessengerProfileType]
                               -> Version
                               -> AccessToken
                               -> Manager
                               -> m (Response FB.SuccessResponse FB.ErrorDetails)
deleteMessengerProfileRequest' mptypes version =
    fbDeleteRequest version "me/messenger_profile" [] go
  where
    go = object [ "fields" .= (show <$> mptypes) ]

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
    metricRequest' mmtypes since until' version3'2

-- | Same as 'metricRequest', but you can set the version of the API.
metricRequest' :: (MonadIO m, MonadThrow m)
               => [MessagingMetricsType]
               -> Maybe POSIXTime
               -> Maybe POSIXTime
               -> Version
               -> AccessToken
               -> Manager
               -> m (Response MetricResponse FB.ErrorDetails)
metricRequest' mmtypes since until' version =
    fbGetRequest version "me/insights/" parameters
  where
    parameters = ("metric", Just metrics) : other
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
featureReviewRequest = featureReviewRequest' version3'2

-- | Same as 'featureReviewRequest', but you can set the version of the API.
featureReviewRequest' :: (MonadIO m, MonadThrow m)
                      => Version
                      -> AccessToken
                      -> Manager
                      -> m (Response FeatureReviewResponse FB.ErrorDetails)
featureReviewRequest' version = fbGetRequest version "me/messaging_feature_review" []

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
