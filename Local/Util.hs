module Local.Util where

import XMonad.ManageHook
import qualified XMonad.StackSet as W
import XMonad.Util.WorkspaceCompare
import Data.List
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
