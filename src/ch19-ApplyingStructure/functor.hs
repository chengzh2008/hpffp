import Data.Time.Clock
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUIDv4
import Web.Scotty



offsetCurrentTime :: NominalDiffTime -> IO UTCTime
offsetCurrentTime offset =
  fmap (addUTCTime (offset * 24 * 3600)) $ getCurrentTime


textUuid :: IO Text
textUuid = fmap (T.pack . UUID.toString) UUIDv4.nextRandom


userAgent :: AppHandler (Maybe UserAgent)
userAgent = (fmap . fmap) userAgent' getRequest

userAgent' :: Request -> Maybe UserAgent
userAgent' req = getHeader "User-Agent" req
