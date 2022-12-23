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

xmobarConfig :: (WorkspaceId -> WindowSet -> WindowSet) -> Prime
xmobarConfig view = do
  handleEventHook =+ serverModeEventHookCmd' $
    flip fmap (asks $ XM.workspaces . XM.config) $
      \wss ->
        ("next-layout",sendMessage NextLayout):
        ("next-ws", doTo Next (Not emptyWS) getSortByIndex' (windows . view)):
        ("prev-ws", doTo Prev (Not emptyWS) getSortByIndex' (windows . view)):
        ("show-title", withNamedWindow $ popup . show):
        [("view" ++ i, windows $ view i) | i <- wss]
  logHook =+ do
    sid <- W.screen . W.current <$> gets windowset
    workspaceNamesPP (myPP sid) >>= dynamicLogString >>= xmonadPropLog

myPP :: ScreenId -> PP
myPP sid = xmobarPP {
      ppCurrent = scrollableWs . xmobarColor "white" "#2b4f98" . pad . xmobarEscape
    , ppVisible = scrollableWs . xmobarColor "black" (if sid == 0 then "#ac9999" else "#9999dd") . pad . clickableWs
    , ppUrgent  = scrollableWs . xmobarColor "red" "yellow" . pad . clickableWs
    , ppHidden  = scrollableWs . xmobarColor "black" "#cccccc" . pad . clickableWs
    , ppLayout =
        doClientCommand 1 "next-layout" . xmobarColor "black" "#cccccc" . pad
    , ppSort = fmap (. filterHidden) (ppSort xmobarPP)
    , ppWsSep    = ""
    , ppSep      = ""
    , ppTitle    = xmobarColor "#999999" "#324c80" . pad . showTitle . xmobarEscape . ellipsize 50
}

ellipsize :: Int -> String -> String
ellipsize n x = case splitAt n x of
  (y, []) -> y
  (y, _) -> take (n-3) y <> "..."

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
