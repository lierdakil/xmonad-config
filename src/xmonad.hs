module Main where

import Control.Concurrent
import Control.Exception
import Control.Monad
import Data.Map qualified as M
import Data.Maybe (fromMaybe)
import Data.Monoid (All(..))
import System.Exit
import System.IO

import XMonad qualified as XM
import XMonad.Actions.CycleWS
import XMonad.Actions.FindEmptyWorkspace
import XMonad.Actions.FlexibleManipulate qualified as Flex
import XMonad.Actions.GridSelect
import XMonad.Actions.Minimize
import XMonad.Actions.MouseGestures
import XMonad.Actions.MouseResize
import XMonad.Actions.Navigation2D
import XMonad.Actions.OnScreen qualified as WW
import XMonad.Actions.WorkspaceNames
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.Minimize
import XMonad.Layout.BinarySpacePartition
import XMonad.Layout.BorderResize
import XMonad.Layout.LayoutCombinators ((|||))
import XMonad.Layout.Minimize
import XMonad.Layout.NoBorders
import XMonad.Layout.Renamed
import XMonad.Prompt
import XMonad.Prompt.Shell
import XMonad.Prompt.XMonad
import XMonad.StackSet qualified as W
import XMonad.Util.Hacks qualified as Hacks
import XMonad.Util.Paste
import XMonad.Util.Replace
import XMonad.Util.Run

import XMonad.Config.Prime.Monadic hiding ((|||))

import Local.DockWindows
import Local.Hue
import Local.Popup
import Local.Util
import Local.Xmobar

import Local.FixEWMH

view :: WorkspaceId -> WindowSet -> WindowSet
view = WW.viewOnScreen 0

main :: IO ()
main = do
 replace
 xmonad $ do
  startWith def{XM.keys = const mempty}

  terminal           =: "wezterm"
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
    ignored =
      [ "desktop_window"
      , "kdesktop"
      , "cairo-dock"
      , "trayer"
      , "xfce4-notifyd"
      ]
  manageHook =+
    composeAll
      [ isClass ignored    --> doIgnore
      , isResource ignored --> doIgnore
      , isClass ["jackmix"]  --> doShift hiddenWorkspaceTag
      ]
  -- this logHook raises unmanaged notification windows; this is required for
  -- e.g. wired, because its windows don't self-raise.
  logHook =+ withDisplay $ \dpy -> do
    (_, _, wins) <- io . queryTree dpy =<< asks theRoot
    mapM_ (io . raiseWindow dpy) =<< filterM (runQuery isNotification) wins

  -- note: no fullscreen support; mostly to facilitate capturing chrome windows.
  -- if you need it, the best option is XMonad.Layout.Fullscreen.fullscreenSupport
  handleEventHook =+ minimizeEventHook
  handleEventHook =+ \ev -> do
    let w = ev_window ev
    whenX (className =? "Launchy" `runQuery` w) $ withDisplay $ \d -> io $ do
      setWindowBorderWidth d w 0
      raiseWindow d w
    return (All True)
  handleEventHook =+ Hacks.windowedFullscreenFixEventHook
  handleEventHook =+ Hacks.trayerAboveXmobarEventHook

  apply $ exc . ewmh
  startupHook =+ fixSupportedAtoms
  apply $ exc . docks

  apply $ exc . Hacks.javaHack

  -- startupHook =+ setWMName "LG3D"

  keepDocksAbove
  xmobarConfig view
  popupConfig

  workspaces =: []

  -- Top keys row, usually it's [1..9, 0] but in my case it's these
  -- because of "weird" xkb layout.
  let topkeys = [0x21, 0x40, 0x23, 0x24, 0x25, 0x5e, 0x26, 0x2a, 0x28, 0x29]

  forM_ (zip [1..(9 :: Int)] $ zip3 topkeys "qwertyuio" "asdfghjkl") $ \(i,(c,j,k)) -> do
    let si = show i
    workspaces =+ [si]
    "M1-" <> si ~~ windows (view si)
    "C-M1-" <> si ~~ swapWithCurrent si
    "S-M1-" <> si ~~ windows (W.shift si)
    rawkeys =+ [((mod5Mask, c), swapWithCurrent si)]
    "M5-" <> si ~~ swapWithCurrent si
    "M5-S-" <> si ~~ swapWithCurrent si
    "M5-" <> [j] ~~ windows (W.shift si)
    "M5-" <> [k] ~~ windows (view si)

  mapM_ (\(n, k) -> "M-" <> show n  ~~ sendKey mod1Mask k)
    $ zip [1..(9 :: Int)] [xK_1 .. xK_9]

  workspaces =+ [hiddenWorkspaceTag]
  "M-<Backspace>" ~~ windows . view $ hiddenWorkspaceTag
  "M-S-<Backspace>" ~~ windows . W.shift $ hiddenWorkspaceTag
  "M5-\\" ~~ windows . view $ hiddenWorkspaceTag

  -- M = M4 = RALT
  -- M1 = LALT
  -- M3 = RCTL
  hueToken <- io $ try (read <$> readFile ".hueToken" :: IO String)
  let
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
  "M1-0" ~~ viewEmptyWorkspace
  "M5-;" ~~ viewEmptyWorkspace
  "M1-S-0" ~~ tagToEmptyWorkspace
  "M5-p" ~~ tagToEmptyWorkspace

  "M-S-<Return>" ~~ spawn =<< asks (XM.terminal . XM.config)
  "M5-<Return>" ~~ spawn =<< asks (XM.terminal . XM.config)

  -- launch program
  "M-p" ~~ shellPrompt myXPConfig

  "M1-S--" ~~ goToSelected def
  "M5-'" ~~ safeSpawn "rofi" ["-show", "window"]
  "M1-S-=" ~~ bringSelected def
  "M5--" ~~ bringSelected def

  "M3-p" ~~ safeSpawn "rofi" ["-show", "combi"]

  "M3-S-p" ~~ safeSpawn "rofi" ["-show", "drun"]

  -- close focused window
  "M1-S-c" ~~ kill
  "M5-c" ~~ kill

  -- Rotate through the available layout algorithms
  "M-<Space>" ~~ sendMessage NextLayout
  "M5-<Space>" ~~ sendMessage NextLayout

  -- Resize viewed windows to the correct size
  "M-n" ~~ refresh

  -- Move focus to the next window
  "M-j" ~~ windows W.focusUp
  "M5-," ~~ windows W.focusUp
  "M1-S-<Tab>" ~~ windows W.focusUp

  -- Move focus to the previous window
  "M-k" ~~ windows W.focusDown
  "M5-." ~~ windows W.focusDown
  "M1-<Tab>" ~~ windows W.focusDown

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

  -- Show dmenu with some XMonad actions
  "M-o" ~~ xmonadPrompt myXPConfig

  -- Toggle the status bar gap
  "M-b" ~~ sendMessage ToggleStruts

  -- Quit xmonad
  "M-S-q" ~~ io exitSuccess

  -- Restart xmonad
  "M-q" ~~ spawn "xmonad --restart"

  -- utility bindings ported from xbindkeysrc
  -- mod3Mask = R_CTRL, see xmodmap
  "<XF86Sleep>"              ~~ spawn "loginctl lock-session"
  "<XF86AudioLowerVolume>"   ~~ spawn "pactl set-sink-volume @DEFAULT_SINK@ -2000"
  "<XF86AudioRaiseVolume>"   ~~ spawn "pactl set-sink-volume @DEFAULT_SINK@ +2000"
  "<Pause>"                  ~~ spawn "apod.ts"
  "<XF86AudioMute>"          ~~ spawn "pactl set-sink-mute @DEFAULT_SINK@ toggle"
  "<XF86AudioNext>"          ~~ spawn "mpc next"
  "<XF86AudioPrev>"          ~~ spawn "mpc prev"
  "<XF86AudioStop>"          ~~ spawn "mpc stop"
  "<XF86AudioPlay>"          ~~ spawn "mpc toggle"
  "M1-<F3>"                  ~~ spawn "xkill"
  "M3-r"                     ~~ spawn "mklink.sh"
  "M3-<F11>"                 ~~ lightsPower "1" False
  "M3-<F12>"                 ~~ lightsPower "1" True
  "M3-S-<F11>"               ~~ lightsPower "2" False
  "M3-S-<F12>"               ~~ lightsPower "2" True
  "M3-<Space>"               ~~ spawn "wired -s 20" -- show notification history

  rawkeys =+ [((mod3Mask, k), lightsBrightness v)
    | (v,k) <- zip (map (floor . (*254)) [0.1,0.2..1 :: Float]) topkeys]
  rawkeys =+ [((mod3Mask .|. shiftMask, k), lightsCt v)
    | (v,k) <- zip (reverse [153,186,219,252,285,318,351,384,417,454]) topkeys]

  -- razer blackwidow macro keys
  "<XF86Tools>"              ~~ spawn "toggle-second-monitor"
  "<XF86Launch5>"            ~~ swapNextScreen
  "<XF86Launch6>"            ~~ spawn "toggle-primary-monitor"
  "<XF86Launch7>"            ~~ spawn "systemctl --user restart kanata"
  "<XF86Launch8>"            ~~ spawn "toggle-third-monitor"
  "<XF86Launch9>"            ~~ spawn "xclip -o | sanskrit-iast -f- | xclip -i -selection clipboard"

  -- I don't use these often, except rotate
  "M3-j"      ~~ sendMessage (ExpandTowards L)
  "M3-l"     ~~ sendMessage (ExpandTowards R)
  "M3-i"        ~~ sendMessage (ExpandTowards U)
  "M3-,"      ~~ sendMessage (ExpandTowards D)
  "M3-u"      ~~ mapM_ (sendMessage . ExpandTowards) [L, U]
  "M3-o"   ~~ mapM_ (sendMessage . ExpandTowards) [R, U]
  "M3-." ~~ mapM_ (sendMessage . ExpandTowards) [R, D]
  "M3-n"       ~~ mapM_ (sendMessage . ExpandTowards) [L, D]
  "M3-k"     ~~ sendMessage Rotate
  -- these I *should* use more often
  "M3-;"     ~~ sendMessage Rotate
  "M3-["    ~~ sendMessage FocusParent
  "M3-/"    ~~ sendMessage Equalize
  "M3-]"     ~~ sendMessage Balance
  --
  "M--"      ~~ withFocused minimizeWindow
  "M-S--"    ~~ withLastMinimized maximizeWindow

  apply $ exc . navigation2DP def
           ("<Up>", "<Left>", "<Down>", "<Right>")
           [("M3-",   windowGo  ),
            ("M3-S-", windowSwap),
            ("M5-",   windowGo  ),
            ("M5-S-", windowSwap)]
           False

  "M1-r" ~~ do
    name <- getWorkspaceNames' <*> gets (W.currentTag . windowset)
    renameWorkspace myXPConfig{ fgColor="brown"
                              , defaultText = drop 1 . dropWhile (/=':') $ fromMaybe "" name}

  "M-<Tab>" ~~ nextScreen
  "M-S-<Tab>" ~~ shiftNextScreen

  let
    gestures = M.fromList
      [ ([L, U], XM.float)
      , ([R, D], windows . W.sink)
      , ([U], (>> windowSwap U True) . focus)
      , ([D], (>> windowSwap D True) . focus)
      , ([R], (>> windowSwap R True) . focus)
      , ([L], (>> windowSwap L True) . focus)
      , ([L, D], (>> windows W.swapMaster) . focus)
      , ([D, R], (>> kill) . focus)
      ]

  mouseBindings =+
        [ ((mod4Mask, 1), \w -> focus w >> asks display >>= io . flip raiseWindow w >> Flex.mouseWindow Flex.discrete w)
        , ((0, 12), mouseGesture gestures)
        , ((mod4Mask, 5), modifyWindowOpacity 0x1fffffff)
        , ((mod4Mask, 4), modifyWindowOpacity (-0x1fffffff))
        ]
