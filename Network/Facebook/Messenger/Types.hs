module Network.Facebook.Messenger.Types where


import           Data.Text      (Text (..))


type UserID           = Text
type AccesToken       = Text
type AccountLinkToken = Text

data FBResponse a b = FBResponse a
                    | FailureResponse b
                    | BadResponse
                        { success_parse_fail :: Text
                        , error_parse_fail   :: Text
                        , original_response  :: Text
                        }

data UserProfileType = FirstName
                     | LastName
                     | ProfilePic
                     | Locale
                     | Timezone
                     | Gender

instance Show UserProfileType where
    show FirstName   = "first_name"
    show LastName    = "last_name"
    show ProfilePic  = "profile_pic"
    show Locale      = "locale"
    show Timezone    = "timezone"
    show Gender      = "gender"
