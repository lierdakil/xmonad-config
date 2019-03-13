{-# LANGUAGE ScopedTypeVariables #-}

module Local.Util where

import XMonad.ManageHook
import qualified XMonad.StackSet as W
import XMonad.Util.WorkspaceCompare
import Data.List
import Data.Maybe
import Foreign.C.Types
import XMonad.Config.Prime.Monadic

anyQ :: Monad m => (a -> m Bool) -> [a] -> m Bool
anyQ f = foldr1 (<||>) . map f

isClass :: [String] -> Query Bool
isClass = anyQ (className =?)

isResource :: [String] -> Query Bool
isResource = anyQ (resource =?)

hiddenWorkspaceTag :: String
hiddenWorkspaceTag = "Hidden"

filterHidden :: [W.Workspace String l a] -> [W.Workspace String l a]
filterHidden = filter $ (/= hiddenWorkspaceTag) . W.tag

mkWsSort' :: X WorkspaceCompare -> X WorkspaceSort
mkWsSort' cmpX = do
  cmp <- cmpX
  return $ sortBy (\a b -> cmp (W.tag a) (W.tag b)) . filterHidden

getSortByIndex' :: X WorkspaceSort
getSortByIndex' = mkWsSort' getWsCompare

infixr 0 ~~
(~~) :: String -> X () -> Prime
bind ~~ act = keys =+ [(bind, act)]

modifyWindowOpacity :: CInt -> Window -> X ()
modifyWindowOpacity delta w = withDisplay $ \d -> do
  atom_NET_WM_WINDOW_OPACITY <- getAtom "_NET_WM_WINDOW_OPACITY"
  opacity :: CUInt <- fromMaybe maxBound . safeHead . fromMaybe []
         <$> io (rawGetWindowProperty 32 d atom_NET_WM_WINDOW_OPACITY w)
  let newOpacity
       -- inequalities may look weird, but that's algebraically equivalent to
       -- what you expect except avoids overflow
       | delta >= 0, opacity > maxBound - delta'
       = maxBound
       | delta < 0, opacity < minBound - delta'
       = minBound
       | otherwise = opacity + delta'
      delta' = fromIntegral delta
  format_cardinal <- getAtom "CARDINAL"
  io $ changeProperty32 d w atom_NET_WM_WINDOW_OPACITY format_cardinal
        propModeReplace [fromIntegral newOpacity]

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:_) = Just x
