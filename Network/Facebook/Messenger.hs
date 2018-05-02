module Network.Facebook.Messenger (
  messageRequest
  , senderActionRequest
  , profileRequest
  , userProfileRequest
  , psidRequest
  , accountUnlinkRequest
  , UserID
  , AccessToken
  , AccountLinkToken
  , Response (..)
  , UserProfileType (..)
  ) where

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Catch (MonadThrow)

import Data.List as List (intercalate)
import Data.String (fromString)
import Data.Text as Text (unpack)
import Data.Text.Encoding as TE (encodeUtf8)
import Network.HTTP.Client (Manager)

import qualified Web.Facebook.Messenger as FB
import Network.Facebook.Messenger.Internal
import Network.Facebook.Messenger.Types


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

profileRequest :: (MonadIO m, MonadThrow m)
               => FB.ProfileRequest
               -> AccessToken
               -> Manager
               -> m (Response FB.SuccessResponse FB.ErrorDetails)
profileRequest = fbPostRequest "me/thread_settings" []

userProfileRequest :: (MonadIO m, MonadThrow m)
                   => [UserProfileType]
                   -> UserID
                   -> AccessToken
                   -> Manager
                   -> m (Response FB.UserProfileResponse FB.ErrorDetails)
userProfileRequest uptypes userid =
    fbGetRequest (Text.unpack userid) [("fields", Just $ typesToBS uptypes)]
  where
    typesToBS = fromString . List.intercalate "," . fmap show

psidRequest :: (MonadIO m, MonadThrow m)
            => AccessToken
            -> AccountLinkToken
            -> Manager
            -> m (Response FB.AccountLinkingResponse FB.ErrorDetails)
psidRequest accountLinkToken =
    fbGetRequest "me" [("fields" , Just "recipient")
                      ,("account_linking_token", Just $ TE.encodeUtf8 accountLinkToken)
                      ]

accountUnlinkRequest :: (MonadIO m, MonadThrow m)
                     => FB.AccountUnlinkRequest
                     -> AccessToken
                     -> Manager
                     -> m (Response FB.SuccessResponse FB.ErrorDetails)
accountUnlinkRequest = fbPostRequest "me/unlink_accounts" []
