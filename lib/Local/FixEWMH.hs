module Local.FixEWMH where

import XMonad

fixSupportedAtoms :: X ()
fixSupportedAtoms = withDisplay $ \dpy -> do
    r <- asks theRoot
    a <- getAtom "_NET_SUPPORTED"
    c <- getAtom "ATOM"
    supp <- mapM getAtom [ "_NET_WM_STATE"
                         , "_NET_WM_STATE_DEMANDS_ATTENTION"
                         ]
    io $ changeProperty32 dpy r a c propModeAppend (fmap fromIntegral supp)
