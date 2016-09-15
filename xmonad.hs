--
-- xmonad example config file.
--
-- A template showing all available configuration hooks,
-- and how to override the defaults in your own xmonad.hs conf file.
--
-- Normally, you'd only override those defaults you care about.
--
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE TupleSections    #-}
{-# LANGUAGE CPP    #-}
{-# LANGUAGE DisambiguateRecordFields    #-}

module Main where

import           Control.Monad                      hiding ((>>))
import           Data.List                          as L
import qualified Data.Map                           as M
import           Data.Monoid
import           System.Exit
import qualified XMonad                             as XM
import           XMonad.Actions.CycleWS
import           XMonad.Actions.FindEmptyWorkspace
import qualified XMonad.Actions.FlexibleManipulate  as Flex
import           XMonad.Actions.MouseGestures
import           XMonad.Config.Prime.Monadic        hiding ((-->), (|||))
import qualified XMonad.Config.Prime.Monadic        as C
import           XMonad.Hooks.DynamicLog
import           XMonad.Hooks.EwmhDesktops
import           XMonad.Hooks.ManageDocks
import           XMonad.Hooks.Minimize
import           XMonad.Hooks.ServerMode
import           XMonad.Layout.BinarySpacePartition
import           XMonad.Layout.BorderResize
import           XMonad.Layout.LayoutCombinators    ((|||))
import qualified XMonad.Layout.LayoutCombinators    as LC
import           XMonad.Layout.Minimize
import           XMonad.Layout.MosaicAlt
import           XMonad.Layout.NoBorders
import           XMonad.Prompt as Pr
import           XMonad.Prompt.Shell
import           XMonad.Prompt.Window
import           XMonad.Prompt.XMonad
import qualified XMonad.StackSet                    as W
import qualified XMonad.Util.ExtensibleState        as XS

import           Local.DockWindows
import qualified Local.Lifx                         as Lifx


-- import qualified Prelude                            as P
import           XMonad.Actions.Navigation2D
import           XMonad.Hooks.UrgencyHook
-- import XMonad.Actions.SwapWorkspaces
import           System.IO
import           XMonad.Actions.WorkspaceNames
import           XMonad.Util.Run
import           XMonad.Layout.Fullscreen (fullscreenSupport)
import           XMonad.Layout.Renamed
import XMonad.Util.WorkspaceCompare
-- import XMonad.Hooks.SetWMName
import System.Environment
import Data.Maybe
import XMonad.Util.Loggers
import System.Process.Internals (translate)
import XMonad.Util.NamedWindows
import XMonad.Util.Dzen as DZ
import Control.Exception
import Control.Concurrent
import Data.IORef
import Control.Arrow

newtype MyUrgencyHook = MyUrgencyHook { popup :: String -> X () }

instance UrgencyHook MyUrgencyHook where
  urgencyHook MyUrgencyHook { popup = pop } w =
      withWindowSet . (. W.findTag w) . flip whenJust . flash =<< getName w
    where
      flash name index =
        pop (show name ++ " requests your attention on workspace " ++ index)

------------------------------------------------------------------------
-- Now run xmonad with all the defaults we set up.

-- Run xmonad with the settings you specify. No need to modify this.

--
--import Prelude

main :: IO ()
main = xmonad $ do
  startWith def{XM.keys = const def}

  terminal           =: "urxvtc"
  focusFollowsMouse  =: False
  clickJustFocuses   =: True
  modMask            =: mod4Mask
  normalBorderColor  =: "#dddddd"
  focusedBorderColor =: "#ff00fe"
  borderWidth        =: 1

  --
  -- mod-[1..9], Switch to workspace N
  -- mod-shift-[1..9], Move client to workspace N
  --
  withWorkspaces $ do
    wsKeys =: map show [1..9 :: Int]
    wsActions =+ [("M1-", windows . W.greedyView)]
    wsActions =+ [("C-M1-", swapWithCurrent)]
    wsActions =+ [("S-M1-", windows . W.shift)]
    -- wsSetName 1 "mail"

  let hiddenWorkspaceTag = "Hidden"
      filterHidden = filter $ (/= hiddenWorkspaceTag) . W.tag

  workspaces =+ [hiddenWorkspaceTag]
  "M-<Backspace>" ~~ windows . W.greedyView $ hiddenWorkspaceTag
  "M-S-<Backspace>" ~~ windows . W.shift $ hiddenWorkspaceTag

  -- M = M4 = RALT
  -- M1 = LALT
  -- M3 = RCTL
  lifxToken <- io $ try (read <$> readFile ".lifxToken")
  let
    layout' f = withWindowSet $ f . description . W.layout . W.workspace . W.current
    lifxPower :: Lifx.PowerState -> X ()
    lifxPower s = lifxCommand $ Lifx.group "Room" . Lifx.powerState s
    lifxBrightness :: Float -> X ()
    lifxBrightness v = lifxCommand $ Lifx.group "Room" . Lifx.brightness v
    lifxCommand :: Lifx.Lifx a => (Lifx.LifxCommand a -> Lifx.LifxCommand a) -> X ()
    lifxCommand c
      | Right tok <- lifxToken = io $ void . forkIO . void $ Lifx.command tok c
      | Left err <- lifxToken = io $ hPrint stderr (err :: SomeException)


    myXPConfig :: XPConfig
    myXPConfig = amberXPConfig{ Pr.font="xft:Fira Mono:pixelsize=24"
                              , position=Top
                              , height=32}
    myXPConfigTerm = myXPConfig {fgColor="green"}
  "M1-0" ~~ viewEmptyWorkspace
  "M1-S--" ~~ windowPromptGoto myXPConfig
  "M1-S-=" ~~ windowPromptBring myXPConfig
  "M1-S-0" ~~ tagToEmptyWorkspace

  -- [((controlMask .|. mod1Mask, k), spawn $ "chvt "++show (i::Int))
  --     | (i,k) <- zip [1..12] [xK_F1..xK_F12] ]
  -- ++

  "M-S-<Return>" ~~ spawn =<< asks $ XM.terminal . XM.config

  -- launch program
  "M-p" ~~ shellPrompt myXPConfig

  -- launch in terminal
  "M-S-p" ~~ flip prompt myXPConfigTerm . (++ " -e") =<< asks $ XM.terminal . XM.config

  -- close focused window
  "M1-S-c" ~~ kill

  -- Rotate through the available layout algorithms
  "M-<Space>" ~~ sendMessage NextLayout

  -- Resize viewed windows to the correct size
  "M-n" ~~ refresh

  -- Move focus to the next window
  "M1-<Tab>" ~~ windows W.focusDown

  -- Move focus to the next window
  "M-j" ~~ windows W.focusDown

  -- Move focus to the previous window
  "M-k" ~~ windows W.focusUp
  "M1-S-<Tab>" ~~ windows W.focusUp

  -- Move focus to the master window
  "M-m" ~~ windows W.focusMaster

  -- Swap the focused window and the master window
  "M-<Return>" ~~ windows W.swapMaster

  -- Swap the focused window with the next window
  "M-S-j" ~~ windows W.swapDown

  -- Swap the focused window with the previous window
  "M-S-k" ~~ windows W.swapUp

  -- Push window back into tiling
  "M-t" ~~ withFocused (windows . W.sink)

  -- layout-dependent keys
  -- Shrink the master area
  "M-h" ~~ layout' $
    \case "MosaicAlt" -> withFocused (sendMessage . shrinkWindowAlt)
          _ -> sendMessage Shrink

  -- Expand the master area
  "M-l" ~~ layout' $
    \case "MosaicAlt" -> withFocused (sendMessage . expandWindowAlt)
          _ -> sendMessage Expand

  -- Increment the number of windows in the master area
  "M-." ~~ layout' $
    \case "MosaicAlt" -> withFocused (sendMessage . tallWindowAlt)
          _ -> sendMessage (IncMasterN 1)

  -- Deincrement the number of windows in the master area
  "M-," ~~ layout' $
    \case "MosaicAlt" -> withFocused (sendMessage . tallWindowAlt)
          _ -> sendMessage (IncMasterN (-1))

  -- Show dmenu with some XMonad actions
  "M-o" ~~ xmonadPrompt myXPConfig

  -- Toggle the status bar gap
  "M-b" ~~ sendMessage ToggleStruts

  -- Quit xmonad
  "M-S-q" ~~ io exitSuccess

  -- Restart xmonad
  "M-q" ~~ spawn "cd ~/.xmonad && stack install && cd ~ && xmonad --restart"

  --mosaic
  --, ((modm, xK_a), sendMessage Taller)
  --, ((modm, xK_z), sendMessage Wider)
  -- !!!
  "M-S-<Space>" ~~ layout' $
    \case "MosaicAlt" -> sendMessage resetAlt
          d -> XM.asks XM.config
               >>=  setLayout . XM.layoutHook
               >> sendMessage (LC.JumpToLayout d)

  -- utility bindings ported from xbindkeysrc
  -- mod3Mask = R_CTRL, see xmodmap
  "<XF86Sleep>"              ~~ spawn "loginctl lock-session $XDG_SESSION_ID"
  "<XF86AudioLowerVolume>"   ~~ spawn "/home/livid/bin/pavol -2000"
  "<XF86AudioRaiseVolume>"   ~~ spawn "/home/livid/bin/pavol +2000"
  "<Pause>"                  ~~ spawn "/home/livid/bin/apod.sh"
  "M-<XF86AudioMute>"        ~~ spawn "/home/livid/bin/pamoveallto"
  "<XF86AudioMute>"          ~~ spawn "/home/livid/bin/pavol mute"
  "<XF86AudioNext>"          ~~ spawn "mpc next"
  "<XF86AudioPrev>"          ~~ spawn "mpc prev"
  "<XF86AudioStop>"          ~~ spawn "mpc stop"
  "<XF86AudioPlay>"          ~~ spawn "mpc toggle"
  "M1-<F3>"                  ~~ spawn "xkill"
  "M3-r"                     ~~ spawn "/home/livid/bin/mklink.sh"
  "M3-s"                     ~~ spawn "/home/livid/bin/screencast"
  "M3-l"                     ~~ spawn "/home/livid/bin/mlock"
  "M3-<F11>"                 ~~ lifxPower Lifx.Off
  "M3-<F12>"                 ~~ lifxPower Lifx.On
  "M3-m"                     ~~ spawn "gajim-remote show_next_pending_event"
  "M3-<F6>"                  ~~ spawn "toggle-touchpad"
  "<Print>"                  ~~ spawn "screenshot"

  keys =+ [("M3-" ++ k, lifxBrightness v) | (v,k) <- zip [0.1,0.2..1] $ map show ([1..9 :: Int]++[0])]

  -- razer blackwidow macro keys
  "<XF86Tools>"              ~~ spawn "winusb -mjk"
  -- "<XF86Launch5>"            ~~ spawn "true"
  -- "<XF86Launch6>"            ~~ spawn "true"
  -- "<XF86Launch7>"            ~~ spawn "true"
  "<XF86Launch8>"            ~~ spawn "/home/livid/bin/pamoveallto"

  "C-<KP_Left>"      ~~ sendMessage (ExpandTowards L)
  "C-<KP_Right>"     ~~ sendMessage (ExpandTowards R)
  "C-<KP_Up>"        ~~ sendMessage (ExpandTowards U)
  "C-<KP_Down>"      ~~ sendMessage (ExpandTowards D)
  "M3-<KP_Left>"  ~~ sendMessage (ShrinkFrom R)
  "M3-<KP_Right>" ~~ sendMessage (ShrinkFrom L)
  "M3-<KP_Up>"    ~~ sendMessage (ShrinkFrom D)
  "M3-<KP_Down>"  ~~ sendMessage (ShrinkFrom U)
  "C-<KP_Home>"      ~~ mapM_ (sendMessage . ExpandTowards) [L, U]
  "C-<KP_Page_Up>"   ~~ mapM_ (sendMessage . ExpandTowards) [R, U]
  "C-<KP_Page_Down>" ~~ mapM_ (sendMessage . ExpandTowards) [R, D]
  "C-<KP_End>"       ~~ mapM_ (sendMessage . ExpandTowards) [L, D]
  "C-<KP_Begin>"     ~~ sendMessage Rotate
  "C-<KP_Delete>"    ~~ sendMessage FocusParent
  "C-<KP_Insert>"    ~~ sendMessage Equalize
  "C-<KP_Enter>"     ~~ sendMessage Balance
  -- "M-s"            ~~ sendMessage Swap
  -- "M-C-n"         ~~ sendMessage SelectNode
  -- "M-S-n"          ~~ sendMessage MoveNode
  "M--"      ~~ withFocused minimizeWindow
  "M-S--"    ~~ sendMessage RestoreNextMinimizedWin

  apply $ navigation2DP def
           ("<Up>", "<Left>", "<Down>", "<Right>")
           [("M3-",   windowGo  ),
            ("M3-S-", windowSwap)]
           False

  "M1-r" ~~ do
    name <- getWorkspaceNames <*> gets (W.currentTag . windowset)
    renameWorkspace myXPConfig{ fgColor="brown"
                              , defaultText = drop 1 . dropWhile (/=':') $ name}

  let
    gestures = M.fromList [
          ([], return $ spawn "toggle-scroll-emulation")
        , ([D], windows . W.sink)
        , ([R], (>> windows W.swapDown) . focus)
        , ([L], (>> windows W.swapUp) . focus)
        , ([L, D], (>> windows W.swapMaster) . focus)
        , ([D, R], (>> kill) . focus)
      ]


  mouseBindings =+
        [ ((0, 9), mouseGesture gestures)
        , ((0, 8), \w -> focus w >> Flex.mouseWindow Flex.discrete w)
        ]

  -- hooks, layouts
  resetLayout $ emptyBSP LC.||| Full
  modifyLayout Prelude.$ squash $ renamed [CutWordsLeft 1] . minimize . borderResize . smartBorders . avoidStruts
  let
    floats =
      [ "baka-mplayer"
      , "Gajim"
      , "Screengrab"
      , "Display"
      ]
    docks =
      [ "Cairo-dock"
      , "Avant-window-navigator"
      ]
    ignored =
      [ "desktop_window"
      , "kdesktop"
      , "cairo-dock"
      ]
    anyQ f = foldr1 (<||>) . map f
    isFloat = anyQ (className =?) floats
    isDock  = anyQ (className =?) docks
    isIgnored = anyQ (resource =?) ignored
  manageHook =+
    composeAll
      [ isFloat   --> doFloat
      , isIgnored --> doIgnore
      ]
  manageHook =+
      checkDock <&&> isDock -->
        do
          win <- ask
          _ <- liftX $ XS.modify $ DockWindows . (win :) . unDockWindows
          doIgnore
  manageHook =+ manageDocks


  handleEventHook =:
    \case
      MapNotifyEvent{} -> do
        d <- asks display
        DockWindows windows' <- XS.get
        mapM_ (io . raiseWindow d) windows'
        >> return (All True)
      DestroyWindowEvent{ev_window=win} ->
        XS.modify (DockWindows . (\\ [win]) . unDockWindows)
        >> return (All True)
      _ -> return (All True)
  let mkWsSort' :: X WorkspaceCompare -> X WorkspaceSort
      mkWsSort' cmpX = do
        cmp <- cmpX
        return $ sortBy (\a b -> cmp (W.tag a) (W.tag b)) . filterHidden
      getSortByIndex' :: X WorkspaceSort
      getSortByIndex' = mkWsSort' getWsCompare
  handleEventHook =+ serverModeEventHookCmd' $
    flip fmap (asks $ XM.workspaces . XM.config) $
      \wss ->
        ("next-layout",sendMessage NextLayout):
        ("next-ws", doTo Next NonEmptyWS getSortByIndex' (windows . W.greedyView)):
        ("prev-ws", doTo Prev NonEmptyWS getSortByIndex' (windows . W.greedyView)):
        ("show-title", withNamedWindow $ popup . show):
        [("view" ++ i, windows $ W.view i) | i <- wss]
  handleEventHook =+ fullscreenEventHook
  handleEventHook =+ minimizeEventHook
  handleEventHook =+ serverModeEventHookF "XMONAD_POPUP" popup
  -- handleEventHook =+ timerEventHook

  apply $ withUrgencyHook $ MyUrgencyHook popup
  -- apply $ withUrgencyHook NoUrgencyHook
  apply ewmh
  apply' fullscreenSupport

  -- startupHook =+ setWMName "LG3D"

  logHook =+
    let
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
      xmobarEscape [] = []
      xmobarEscape s = "<raw="++len++":"++s++"/>"
        where len = show $ length s
      -- Wraps a workspace name with a dzen clickable action that focusses that workspace
      showTitle = doClientCommand 1 "show-title"
      scrollableWs =
            doClientCommand 4 "prev-ws"
          . doClientCommand 5 "next-ws"
      clickableWs ws =
        doClientCommand 1 ("view"++takeWhile (/=':') ws) $ xmobarEscape ws
      doClientCommand :: Int -> String -> String -> String
      doClientCommand btn command =
        wrap ("<action=`"++cmdClient++" "++command++"` button="++show btn++">") "</action>"
          where
              cmdClient = "xmonadctl"
    in workspaceNamesPP myPP >>= dynamicLogString >>= xmonadPropLog

  where
    infixr 2 $
    ($) = (Prelude.$)
    infix 2 -->
    (-->) = (C.-->)
    infixr 0 ~~
    x ~~ y = keys =+ [(x, y)]

    popup :: String -> X ()
    popup = dzenConfig popupConfig
      where
        popupConfig = onCurr (vCenter 22) >=> timeout 2 >=> background "darkgreen" >=> DZ.font "xft:Fira Mono:pixelsize=18"
        background color = addArgs ["-bg", color]
