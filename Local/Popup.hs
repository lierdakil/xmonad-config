module Local.Popup where

import XMonad.Util.Dzen
import XMonad.Config.Prime.Monadic
import XMonad.Hooks.UrgencyHook
import XMonad.Util.NamedWindows
import qualified XMonad.StackSet as W
import XMonad.Hooks.ServerMode

newtype MyUrgencyHook = MyUrgencyHook { muhPopup :: String -> X () }

instance UrgencyHook MyUrgencyHook where
  urgencyHook MyUrgencyHook { muhPopup = pop } w =
      withWindowSet . (. W.findTag w) . flip whenJust . flash =<< getName w
    where
      flash name index =
        pop (show name ++ " requests your attention on workspace " ++ index)

popup :: String -> X ()
popup = dzenConfig pc
  where
    pc = onCurr (vCenter 22) >=> timeout 2 >=> background "darkgreen" >=> font "xft:Fira Mono:pixelsize=18"
    background color = addArgs ["-bg", color]

popupConfig :: Prime
popupConfig = do
  handleEventHook =+ serverModeEventHookF "XMONAD_POPUP" popup
  apply $ exc . withUrgencyHook (MyUrgencyHook popup)
