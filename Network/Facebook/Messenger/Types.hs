module Network.Facebook.Messenger.Types where


import           Data.Text          (Text)
import           Data.ByteString    (ByteString)

type UserID           = Text
type AccessToken      = Text
type AccountLinkToken = Text

data FBResponse a b = FBResponse a
                    | FailureResponse b
                    | BadResponse
                        { success_parse_fail :: Text
                        , error_parse_fail   :: Text
                        , original_response  :: ByteString
                        }

data UserProfileType = FirstName
                     | LastName
                     | ProfilePic
                     | Locale
                     | Timezone
                     | Gender
                     | IsPaymentEnabled

instance Show UserProfileType where
    show FirstName   = "first_name"
    show LastName    = "last_name"
    show ProfilePic  = "profile_pic"
    show Locale      = "locale"
    show Timezone    = "timezone"
    show Gender      = "gender"
    show IsPaymentEnabled = "is_payment_enabled"
