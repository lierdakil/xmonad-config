{-# LANGUAGE LambdaCase       #-}

module Main where

import Control.Monad
import System.IO
import Control.Exception
import Control.Concurrent
import System.Exit
import Data.Monoid (All(..))
import qualified Data.Map as M

import qualified XMonad as XM
import qualified XMonad.StackSet as W
import XMonad.Actions.FindEmptyWorkspace
import XMonad.Actions.MouseGestures
import qualified XMonad.Actions.FlexibleManipulate as Flex
import XMonad.Actions.Navigation2D
import XMonad.Actions.WorkspaceNames
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.Minimize
import XMonad.Actions.Minimize
import XMonad.Layout.BinarySpacePartition
import XMonad.Layout.BorderResize
import XMonad.Actions.MouseResize
import XMonad.Layout.LayoutCombinators ((|||))
import qualified XMonad.Layout.LayoutCombinators as LC
import XMonad.Layout.Minimize
import XMonad.Actions.GridSelect
import XMonad.Layout.MosaicAlt
import XMonad.Layout.NoBorders
import XMonad.Layout.Fullscreen (fullscreenSupport)
import XMonad.Layout.Renamed
import XMonad.Prompt
import XMonad.Prompt.Shell
import XMonad.Prompt.Window
import XMonad.Prompt.XMonad
import XMonad.Util.Replace
import XMonad.Util.Run

import XMonad.Config.Prime.Monadic hiding ((|||))

import Local.Hue
import Local.Util
import Local.DockWindows
import Local.Popup
import Local.Xmobar

import Local.FixEWMH

main :: IO ()
main = do
 replace
 xmonad $ do
  startWith def{XM.keys = const def}

  terminal           =: "alacritty"
  focusFollowsMouse  =: False
  clickJustFocuses   =: True
  modMask            =: mod4Mask
  normalBorderColor  =: "#dddddd"
  focusedBorderColor =: "#ff00fe"
  borderWidth        =: 1

  -- hooks, layouts
  resetLayout $ emptyBSP ||| Full
  modifyLayout $ Layout . renamed [CutWordsLeft 1] . minimize . mouseResize . borderResize . smartBorders . avoidStruts
  let
    floats =
      [ "baka-mplayer"
      , "Gajim"
      , "Screengrab"
      , "Display"
      ]
    ignored =
      [ "desktop_window"
      , "kdesktop"
      , "cairo-dock"
      , "trayer"
      , "xfce4-notifyd"
      ]
  manageHook =+
    composeAll
      [ isClass floats     --> doFloat
      , isClass ignored    --> doIgnore
      , isResource ignored --> doIgnore
      ]

  handleEventHook =+ fullscreenEventHook
  handleEventHook =+ minimizeEventHook
  handleEventHook =+ \ev -> do
    let w = ev_window ev
    whenX (className =? "Launchy" `runQuery` w) $ withDisplay $ \d -> io $ do
      setWindowBorderWidth d w 0
      raiseWindow d w
    return (All True)

  apply $ exc . ewmh
  startupHook =+ fixSupportedAtoms
  apply $ exc . docks
  apply $ exc . fullscreenSupport

  -- startupHook =+ setWMName "LG3D"

  keepDocksAbove
  xmobarConfig
  popupConfig

  -- workspaces
  withWorkspaces $ do
    wsKeys =: map show [1..9 :: Int]
    wsActions =+ [("M1-", windows . W.greedyView)]
    wsActions =+ [("C-M1-", swapWithCurrent)]
    wsActions =+ [("S-M1-", windows . W.shift)]
    -- wsSetName 1 "mail"

  workspaces =+ [hiddenWorkspaceTag]
  "M-<Backspace>" ~~ windows . W.greedyView $ hiddenWorkspaceTag
  "M-S-<Backspace>" ~~ windows . W.shift $ hiddenWorkspaceTag

  -- M = M4 = RALT
  -- M1 = LALT
  -- M3 = RCTL
  hueToken <- io $ try (read <$> readFile ".hueToken" :: IO String)
  let
    layout' f = withWindowSet $ f . description . W.layout . W.workspace . W.current
    lightsPower g = lightsCommand g . ParameterOn
    lightsBrightness = lightsCommand lightsGroup . ParameterBrightness
    lightsCt = lightsCommand lightsGroup . ParameterColor
    lightsCommand g c = io $
      case hueToken of
        Right tok -> void . forkIO . void $ command tok g c
        Left err -> hPrint stderr (err :: SomeException)
    lightsGroup = "1"


    myXPConfig :: XPConfig
    myXPConfig = amberXPConfig{ font="xft:Fira Mono:pixelsize=24"
                              , position=Top
                              , height=32}
    myXPConfigTerm = myXPConfig {fgColor="green"}
  "M1-0" ~~ viewEmptyWorkspace
  "M1-S--" ~~ windowPrompt myXPConfig Goto allWindows
  "M1-S-=" ~~ windowPrompt myXPConfig Bring allWindows
  "M1-S-0" ~~ tagToEmptyWorkspace

  -- [((controlMask .|. mod1Mask, k), spawn $ "chvt "++show (i::Int))
  --     | (i,k) <- zip [1..12] [xK_F1..xK_F12] ]
  -- ++

  "M-S-<Return>" ~~ spawn =<< asks (XM.terminal . XM.config)

  -- launch program
  "M-p" ~~ shellPrompt myXPConfig

  -- launch in terminal
  "M-S-p" ~~ do
    term <- asks (XM.terminal . XM.config)
    prompt (term ++ " -e") myXPConfigTerm

  "M3-p" ~~ spawnSelected def [ "spacefm"
                              , "google-chrome-stable"
                              , "seahorse"
                              , "urxvt"
                              , "atom"
                              , "krita"
                              ]

  "M3-S-p" ~~ safeSpawn "dmenu-xdg" ["-l", "15"]

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

  -- Grid select
  "C-M1-<Tab>" ~~ goToSelected def

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
  "M-S-t" ~~ withFocused XM.float

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
  "M-S-f" ~~ restart "/home/livid/bin/switchWM" True

  -- Restart xmonad
  "M-q" ~~ spawn "xmonad --recompile; xmonad --restart"

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
  "<XF86Sleep>"              ~~ spawn "loginctl lock-session"
  "<XF86AudioLowerVolume>"   ~~ spawn "pavol -2000"
  "<XF86AudioRaiseVolume>"   ~~ spawn "pavol +2000"
  "<Pause>"                  ~~ spawn "apod.sh"
  "M-<XF86AudioMute>"        ~~ spawn "pamoveallto"
  "<XF86AudioMute>"          ~~ spawn "pavol mute"
  "<XF86AudioNext>"          ~~ spawn "mpc next"
  "<XF86AudioPrev>"          ~~ spawn "mpc prev"
  "<XF86AudioStop>"          ~~ spawn "mpc stop"
  "<XF86AudioPlay>"          ~~ spawn "mpc toggle"
  "M1-<F3>"                  ~~ spawn "xkill"
  "M3-r"                     ~~ spawn "mklink.sh"
  "M3-t"                     ~~ spawn "shlink.sh"
  "M3-s"                     ~~ spawn "screencast"
  "M3-l"                     ~~ spawn "mlock"
  "M3-<F11>"                 ~~ lightsPower "1" False
  "M3-<F12>"                 ~~ lightsPower "1" True
  "M3-S-<F11>"               ~~ lightsPower "2" False
  "M3-S-<F12>"               ~~ lightsPower "2" True
  "M3-/"                     ~~ spawn "hexchat -e -c 'gui show'"
  "M3-S-/"                   ~~ spawn "hexchat -e -c 'gui hide'"
  "M3-<F6>"                  ~~ spawn "toggle-touchpad"
  "<Print>"                  ~~ spawn "screenshot"
  "M3-S-<Backspace>"         ~~
    lines <$> runProcessWithInput "pidof" ["deadd-notification-center"] [] >>=
    safeSpawn "kill" . ("-USR1":)

  keys =+ [("M3-" ++ k, lightsBrightness v) | (v,k) <- zip (map (floor . (*254)) [0.1,0.2..1 :: Float]) $ map show ([1..9 :: Int]++[0])]
  keys =+ [("M3-S-" ++ k, lightsCt v) | (v,k) <- zip [153,186,219,252,285,318,351,384,417,454] $ map show ([1..9 :: Int]++[0])]

  -- razer blackwidow macro keys
  -- "<XF86Tools>"              ~~ spawn "winusb -mjk"
  -- "<XF86Launch5>"            ~~ spawn "true"
  -- "<XF86Launch6>"            ~~ spawn "true"
  "<XF86Launch7>"            ~~ spawn "bluetooth-connect"
  "<XF86Launch8>"            ~~ spawn "pamoveallto"

  "M3-<KP_Left>"      ~~ sendMessage (ExpandTowards L)
  "M3-<KP_Right>"     ~~ sendMessage (ExpandTowards R)
  "M3-<KP_Up>"        ~~ sendMessage (ExpandTowards U)
  "M3-<KP_Down>"      ~~ sendMessage (ExpandTowards D)
  "M3-<KP_Subtract>"  ~~ sendMessage RotateL
  "M3-<KP_Add>"       ~~ sendMessage RotateR
  "M3-<KP_Home>"      ~~ mapM_ (sendMessage . ExpandTowards) [L, U]
  "M3-<KP_Page_Up>"   ~~ mapM_ (sendMessage . ExpandTowards) [R, U]
  "M3-<KP_Page_Down>" ~~ mapM_ (sendMessage . ExpandTowards) [R, D]
  "M3-<KP_End>"       ~~ mapM_ (sendMessage . ExpandTowards) [L, D]
  "M3-<KP_Begin>"     ~~ sendMessage Rotate
  "M3-<KP_Delete>"    ~~ sendMessage FocusParent
  "M3-<KP_Insert>"    ~~ sendMessage Equalize
  "M3-<KP_Enter>"     ~~ sendMessage Balance
  -- "M-s"            ~~ sendMessage Swap
  -- "M-C-n"         ~~ sendMessage SelectNode
  -- "M-S-n"          ~~ sendMessage MoveNode
  "M--"      ~~ withFocused minimizeWindow
  "M-S--"    ~~ withLastMinimized maximizeWindow

  apply $ exc . navigation2DP def
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
        [ ((0, 8), windows . W.sink)
        , ((0, 9), mouseGesture gestures)
        , ((mod3Mask, 1), \w -> focus w >> asks display >>= io . flip raiseWindow w >> Flex.mouseWindow Flex.discrete w)
        , ((0, 10), return $ spawn "toggle-scroll-emulation")
        , ((0, 11), windows . W.sink)
        , ((0, 12), mouseGesture gestures)
        , ((mod3Mask, 4), modifyWindowOpacity 0x1fffffff)
        , ((mod3Mask, 5), modifyWindowOpacity (-0x1fffffff))
        ]
