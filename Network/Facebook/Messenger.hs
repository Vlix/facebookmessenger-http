module Network.Facebook.Messenger (
  -- * Types
  AccessToken (..)
  , AccountLinkToken (..)
  , UserProfileType (..)
  , Response (..)
  , ParseError (..)
  -- * Requests
  , messageRequest
  , senderActionRequest
  -- ** User Profile
  , userProfileRequest
  -- ** Account Linking
  , psidRequest
  , accountUnlinkRequest
  -- ** Messenger Code
  , messengerCodeRequest
  -- ** Messenger Profile
  , getMessengerProfileRequest
  , setMessengerProfileRequest
  , deleteMessengerProfileRequest
  ) where

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Catch (MonadThrow)

import Data.Aeson ((.=), object)
import Data.List as List (intercalate)
import Data.String (fromString)
import Data.Text as Text (unpack)
import Data.Text.Encoding as TE (encodeUtf8)
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

https://developers.facebook.com/docs/messenger-platform/analytics
https://developers.facebook.com/docs/messenger-platform/reference/messaging-insights-api

https://developers.facebook.com/docs/messenger-platform/reference/id-matching-api

https://developers.facebook.com/docs/messenger-platform/reference/messaging-feature-review-api

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
    let types = fromString $ List.intercalate "," $ show <$> uptypes
    in  fbGetRequest (Text.unpack psid) [("fields", Just types)]


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
    fbGetRequest "me/messenger_profile" [("fields", Just types)]
  where types = fromString $ List.intercalate "," $ show <$> mptypes

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
