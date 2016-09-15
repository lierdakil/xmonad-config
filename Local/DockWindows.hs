{-# LANGUAGE DeriveDataTypeable        #-}
module Local.DockWindows where

import XMonad

newtype DockWindows = DockWindows {unDockWindows :: [Window]}
  deriving (Typeable, Read, Show)

instance ExtensionClass DockWindows where
 initialValue = DockWindows []
 extensionType = PersistentExtension
