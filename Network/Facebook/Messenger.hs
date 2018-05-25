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
  -- ** Messaging Insights
  , metricRequest
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

messageRequest :: (MonadIO m, MonadThrow m)
               => FB.SendRequest
               -> AccessToken
               -> Manager
               -> m (Response FB.MessageResponse FB.ErrorDetails)
messageRequest = fbPostRequest "me/messages" []

senderActionRequest :: (MonadIO m, MonadThrow m)
                    => FB.SenderActionRequest
                    -> AccessToken
                    -> Manager
                    -> m (Response FB.SenderActionResponse FB.ErrorDetails)
senderActionRequest = fbPostRequest "me/messages" []


-- -------------- --
--  USER PROFILE  --
-- -------------- --

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

accountUnlinkRequest :: (MonadIO m, MonadThrow m)
                     => FB.AccountUnlinkRequest
                     -> AccessToken
                     -> Manager
                     -> m (Response FB.SuccessResponse FB.ErrorDetails)
accountUnlinkRequest = fbPostRequest "me/unlink_accounts" []

-- ---------------- --
--  THREAD CONTROL  --
-- ---------------- --

passThreadControlRequest :: (MonadIO m, MonadThrow m)
                         => FB.PassThreadControlRequest
                         -> AccessToken
                         -> Manager
                         -> m (Response FB.ThreadControlResponse FB.ErrorDetails)
passThreadControlRequest = fbPostRequest "me/pass_thread_control" []

requestThreadControlRequest :: (MonadIO m, MonadThrow m)
                            => FB.ThreadControlRequest
                            -> AccessToken
                            -> Manager
                            -> m (Response FB.ThreadControlResponse FB.ErrorDetails)
requestThreadControlRequest = fbPostRequest "me/request_thread_control" []

takeThreadControlRequest :: (MonadIO m, MonadThrow m)
                         => FB.ThreadControlRequest
                         -> AccessToken
                         -> Manager
                         -> m (Response FB.ThreadControlResponse FB.ErrorDetails)
takeThreadControlRequest = fbPostRequest "me/take_thread_control" []

secondaryReceiversRequest :: (MonadIO m, MonadThrow m)
                          => AccessToken
                          -> Manager
                          -> m (Response FB.SecondaryReceiverResponse FB.ErrorDetails)
secondaryReceiversRequest = fbGetRequest "me/secondary_receivers" [("fields", Just "id,name")]

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

messengerCodeRequest :: (MonadIO m, MonadThrow m)
                     => FB.MessengerCodeRequest
                     -> AccessToken
                     -> Manager
                     -> m (Response FB.MessengerCodeResponse FB.ErrorDetails)
messengerCodeRequest = fbPostRequest "me/messenger_codes" []


-- ------------------- --
--  MESSENGER PROFILE  --
-- ------------------- --

getMessengerProfileRequest :: (MonadIO m, MonadThrow m)
                           => [MessengerProfileType]
                           -> AccessToken
                           -> Manager
                           -> m (Response FB.GetProfileResponse FB.ErrorDetails)
getMessengerProfileRequest mptypes =
    fbGetRequest "me/messenger_profile" [("fields", Just $ commaList mptypes)]

setMessengerProfileRequest :: (MonadIO m, MonadThrow m)
                           => FB.ProfileRequest
                           -> AccessToken
                           -> Manager
                           -> m (Response FB.SuccessResponse FB.ErrorDetails)
setMessengerProfileRequest = fbPostRequest "me/messenger_profile" []

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

type MetricResponse = FB.DataResponse MetricValues

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

featureReviewRequest :: (MonadIO m, MonadThrow m)
                     => AccessToken
                     -> Manager
                     -> m (Response FeatureReviewResponse FB.ErrorDetails)
featureReviewRequest = fbGetRequest "me/messaging_feature_review" []

type FeatureReviewResponse = FB.DataResponse FeatureStatus

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
