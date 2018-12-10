{-# LANGUAGE LambdaCase #-}
module Network.Facebook.Messenger.Types where


import Data.Aeson (FromJSON(..), withText)
import Data.Text (Text, unpack)
import Data.ByteString (ByteString)
import Network.HTTP.Types (Status)

import Web.Facebook.Messenger.Internal (withTextCI)


newtype AccessToken = AccessToken Text
newtype AccountLinkToken = AccountLinkToken Text

data Response a b = Response a
                  | FailureResponse Status b
                  | BadResponse Status ParseError

data ParseError = ParseError
    { successParseFail :: Text
    , errorParseFail   :: Text
    , originalResponse  :: ByteString
    } deriving (Eq, Show, Read, Ord)

data UserProfileType = FirstName
                     | LastName
                     | ProfilePic
                     | Locale
                     | Timezone
                     | Gender
                     | IsPaymentEnabled
    deriving (Eq, Ord)

match :: Eq a => [a] -> b -> ([a] -> [(b,[a])]) -> [a] -> [(b,[a])]
match as b f x = go $ splitAt (length as) x
  where go (as2, rest) | as2 == as = [(b,rest)]
                       | otherwise = f x

instance Show UserProfileType where
    show FirstName  = "first_name"
    show LastName   = "last_name"
    show ProfilePic = "profile_pic"
    show Locale     = "locale"
    show Timezone   = "timezone"
    show Gender     = "gender"
    show IsPaymentEnabled = "is_payment_enabled"

instance Read UserProfileType where
    readsPrec _ = match "first_name" FirstName
                $ match "last_name" LastName
                $ match "profile_pic" ProfilePic
                $ match "locale" Locale
                $ match "timezone" Timezone
                $ match "gender" Gender
                $ match "is_payment_enabled" IsPaymentEnabled
                $ const []

data MessengerProfileType = Greeting
                          | GetStarted
                          | PersistentMenu
                          | WhiteListedDomains
                          | AccountLinkingUrl
                          | PaymentSettings
                          | TargetAudience
                          | HomeUrl
    deriving (Eq, Ord)

instance Show MessengerProfileType where
    show Greeting           = "greeting"
    show GetStarted         = "get_started"
    show PersistentMenu     = "profile_pic"
    show WhiteListedDomains = "locale"
    show AccountLinkingUrl  = "timezone"
    show PaymentSettings    = "gender"
    show TargetAudience     = "target_audience"
    show HomeUrl            = "is_payment_enabled"

instance Read MessengerProfileType where
    readsPrec _ = match "greeting" Greeting
                $ match "get_started" GetStarted
                $ match "profile_pic" PersistentMenu
                $ match "locale" WhiteListedDomains
                $ match "timezone" AccountLinkingUrl
                $ match "gender" PaymentSettings
                $ match "target_audience" TargetAudience
                $ match "is_payment_enabled" HomeUrl
                $ const []

data MessagingMetricsType =
      TotalMessagingConnections
    | NewConversationUnique
    | ActiveThreadsUnique
    | BlockedConversationsUnique
    | ReportedConversationsUnique
    | ReportedConversationsByReportTypeUnique
    | FeedbackByActionUnique
  deriving (Eq, Ord)

instance Show MessagingMetricsType where
    show TotalMessagingConnections               = "page_messages_total_messaging_connections"
    show NewConversationUnique                   = "page_messages_new_conversations_unique"
    show ActiveThreadsUnique                     = "page_messages_active_threads_unique"
    show BlockedConversationsUnique              = "page_messages_blocked_conversations_unique"
    show ReportedConversationsUnique             = "page_messages_reported_conversations_unique"
    show ReportedConversationsByReportTypeUnique = "page_messages_reported_conversations_by_report_type_unique"
    show FeedbackByActionUnique                  = "page_messages_feedback_by_action_unique"

instance Read MessagingMetricsType where
  readsPrec _ = match "page_messages_total_messaging_connections" TotalMessagingConnections
              $ match "page_messages_new_conversations_unique" NewConversationUnique
              $ match "page_messages_active_threads_unique" ActiveThreadsUnique
              $ match "page_messages_blocked_conversations_unique" BlockedConversationsUnique
              $ match "page_messages_reported_conversations_unique" ReportedConversationsUnique
              $ match "page_messages_reported_conversations_by_report_type_unique" ReportedConversationsByReportTypeUnique
              $ match "page_messages_feedback_by_action_unique" FeedbackByActionUnique
              $ const []

instance FromJSON MessagingMetricsType where
  parseJSON = withText "MessagingMetricsType" $ \case
      "page_messages_total_messaging_connections" -> pure TotalMessagingConnections
      "page_messages_new_conversations_unique" -> pure NewConversationUnique
      "page_messages_active_threads_unique" -> pure ActiveThreadsUnique
      "page_messages_blocked_conversations_unique" -> pure BlockedConversationsUnique
      "page_messages_reported_conversations_unique" -> pure ReportedConversationsUnique
      "page_messages_reported_conversations_by_report_type_unique" -> pure ReportedConversationsByReportTypeUnique
      "page_messages_feedback_by_action_unique" -> pure FeedbackByActionUnique
      wat -> fail $ "unexpected MessagingMetricsType: " `mappend` unpack wat

data FeatureStatusType =
    PENDING
  | REJECTED
  | APPROVED
  | LIMITED
  deriving (Eq, Show, Read, Ord)

instance FromJSON FeatureStatusType where
  parseJSON = withTextCI "FeatureStatusType"
      [("pending",PENDING)
      ,("rejected",REJECTED)
      ,("approved",APPROVED)
      ,("limited",LIMITED)
      ]
