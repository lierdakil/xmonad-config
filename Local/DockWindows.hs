{-# LANGUAGE DeriveDataTypeable, LambdaCase #-}
module Local.DockWindows (keepDocksAbove) where

import XMonad.Config.Prime.Monadic
import XMonad.Hooks.ManageDocks
import qualified XMonad.Util.ExtensibleState as XS
import Data.Monoid
import Data.List
import Local.Util

newtype DockWindows = DockWindows {unDockWindows :: [Window]}
  deriving (Typeable, Read, Show)

instance ExtensionClass DockWindows where
 initialValue = DockWindows []
 extensionType = PersistentExtension

keepDocksAbove :: Prime
keepDocksAbove = do
  manageHook =+ (
    checkDock <&&> isDock -->
      do
        win <- ask
        _ <- liftX $ XS.modify $ DockWindows . (win :) . unDockWindows
        doIgnore
    )

  handleEventHook =+
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

  where
    docks =
      [ "Cairo-dock"
      , "Avant-window-navigator"
      ]
    isDock  = anyQ (className =?) docks
