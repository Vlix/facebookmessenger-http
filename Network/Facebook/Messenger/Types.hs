module Network.Facebook.Messenger.Types where


import Data.Text (Text)
import Data.ByteString (ByteString)

newtype AccessToken = AccessToken Text
newtype AccountLinkToken = AccountLinkToken Text

data Response a b = Response a
                  | FailureResponse b
                  | BadResponse ParseError

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

match :: Eq a => [a] -> b -> ([a] -> [(b,[a])]) -> [a] -> [(b,[a])]
match as b f x = go $ splitAt (length as) x
  where go (as2, rest) | as2 == as = [(b,rest)]
                       | otherwise = f x

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
