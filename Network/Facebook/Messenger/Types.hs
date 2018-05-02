module Network.Facebook.Messenger.Types where


import Data.Text (Text)
import Data.ByteString (ByteString)

type UserID = Text
type AccessToken = Text
type AccountLinkToken = Text

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
    deriving (Eq, Read, Ord)

instance Show UserProfileType where
    show FirstName   = "first_name"
    show LastName    = "last_name"
    show ProfilePic  = "profile_pic"
    show Locale      = "locale"
    show Timezone    = "timezone"
    show Gender      = "gender"
    show IsPaymentEnabled = "is_payment_enabled"
