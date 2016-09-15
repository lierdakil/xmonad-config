module Local.Xmobar (xmobarConfig) where

import XMonad.Hooks.DynamicLog

import XMonad.Config.Prime.Monadic


import XMonad.Actions.WorkspaceNames
import Local.Util
import Local.Popup
import XMonad.Hooks.ServerMode
import qualified XMonad as XM
import XMonad.Actions.CycleWS
import XMonad.Util.NamedWindows
import qualified XMonad.StackSet as W

xmobarConfig :: Prime
xmobarConfig = do
  handleEventHook =+ serverModeEventHookCmd' $
    flip fmap (asks $ XM.workspaces . XM.config) $
      \wss ->
        ("next-layout",sendMessage NextLayout):
        ("next-ws", doTo Next NonEmptyWS getSortByIndex' (windows . W.greedyView)):
        ("prev-ws", doTo Prev NonEmptyWS getSortByIndex' (windows . W.greedyView)):
        ("show-title", withNamedWindow $ popup . show):
        [("view" ++ i, windows $ W.view i) | i <- wss]
  logHook =+ workspaceNamesPP myPP >>= dynamicLogString >>= xmonadPropLog

myPP :: PP
myPP    = xmobarPP {
      ppCurrent = scrollableWs . xmobarColor "white" "#2b4f98" . pad . xmobarEscape
    , ppVisible = scrollableWs . xmobarColor "black" "#999999" . pad . clickableWs
    , ppUrgent  = scrollableWs . xmobarColor "red" "yellow" . pad . clickableWs
    , ppHidden  = scrollableWs . xmobarColor "black" "#cccccc" . pad . clickableWs
    , ppLayout =
        doClientCommand 1 "next-layout" . xmobarColor "black" "#cccccc" . pad
    , ppSort = fmap (. filterHidden) (ppSort xmobarPP)
    , ppWsSep    = ""
    , ppSep      = ""
    , ppTitle    = xmobarColor "#999999" "#324c80" . pad . showTitle . xmobarEscape
}

xmobarEscape :: String -> String
xmobarEscape [] = []
xmobarEscape s = "<raw="++len++":"++s++"/>"
  where len = show $ length s

showTitle :: String -> String
showTitle = doClientCommand 1 "show-title"

scrollableWs :: String -> String
scrollableWs =
      doClientCommand 4 "prev-ws"
    . doClientCommand 5 "next-ws"

clickableWs :: String -> String
clickableWs ws =
  doClientCommand 1 ("view"++takeWhile (/=':') ws) $ xmobarEscape ws

doClientCommand :: Int -> String -> String -> String
doClientCommand btn command =
  wrap ("<action=`"++cmdClient++" "++command++"` button="++show btn++">") "</action>"
    where
        cmdClient = "xmonadctl"
