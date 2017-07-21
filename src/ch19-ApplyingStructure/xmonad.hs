-- TODO: build failure "Failed to load interface for 'XMonad.Actions.Volume'"
import XMonad
import XMonad.Actions.Volume
import Data.Map.Lazy (fromList)
import Data.Monoid (mappend)

main = do
  xmonad def { keys =
               \c -> fromList [
                 ((0, xK_F7), lowerVolume 4 >> return ()),
                 ((0, xK_F8), raiseVolume 4 >> return ())
                 ] `mappend` keys defaultConfig c }
