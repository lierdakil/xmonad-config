module Local.Popup where

import XMonad.Actions.ShowText
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
popup = flashText cf 2
  where
    cf = def {
        st_font = "xft:Fira Mono:pixelsize=18"
      , st_bg = "darkgreen"
      , st_fg = "white"
      }
    -- pc = onCurr (vCenter 22) >=> timeout 2 >=> background "darkgreen" >=> font "xft:Fira Mono:pixelsize=18"
    -- background color = addArgs ["-bg", color]

popupConfig :: Prime
popupConfig = do
  handleEventHook =+ serverModeEventHookF "XMONAD_POPUP" popup
  handleEventHook =+ handleTimerEvent
  apply $ exc . withUrgencyHookC (MyUrgencyHook popup) muhConfig
  where
    muhConfig = def {
        suppressWhen = Focused
      , remindWhen = Repeatedly 5 5
      }
