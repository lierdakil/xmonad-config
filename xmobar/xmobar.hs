module Main (main) where

import Xmobar
import Paths_xmonad_config
import System.FilePath

config :: FilePath -> Config
config datadir = defaultConfig
  { font = "Fira Code Semi-Bold 16px"
  , borderColor = "black"
  , border = NoBorder
  , bgColor = "#3f3c6d"
  , fgColor = "#a8a3f7"
  , position = TopSize C 100 20
  , commands =
      [ Run UnsafeXMonadLog
      , Run $ Memory ["-t", "<usedipat> <usedratio>%", "--", "--used-icon-pattern", "<icon=hbar_%%.xbm/>"] 100
      , Run $ Swap ["-t", "<usedratio>%"] 100
      , Run $ Cpu ["-t", "<ipat> <total>%", "--", "--load-icon-pattern", "<icon=hbar_%%.xbm/>"] 10
      , Run $ CoreTemp
          ["-t", "<core0>⁰C"
          , "-L", "40", "-H", "70"
          , "-l", "lightblue", "-n", "gray90", "-h", "red"]
          10
      , Run $ DiskU
          [ ("/",                  "<fc=violet>slash: <usedipat> <usedp>%</fc> | ")
          , ("/home",              "<fc=grey>home: <usedipat> <usedp>%</fc>")
          ]
          ["--", "--used-icon-pattern", "<icon=hbar_%%.xbm/>"]
          100
      , Run $ Com "date" ["+%c"] "date" 10
      , Run $ TopProc ["-t", "<name1>"] 100
      , Run $ TopMem ["-t", "<name1>"] 100
      , Run $ Kbd []
      , Run $ Com "xinput-mouse-state" [] "mstate" 1
      ]
  , lowerOnStart = True
  , overrideRedirect = True
  , sepChar = "%"
  , iconRoot = datadir </> "xmobar" </> "icons"
  , alignSep = "}{"
  , template = "\
      \%UnsafeXMonadLog% \
      \}{ <fc=brown>%cpu% %top%</fc> \
      \| %coretemp% \
      \| <fc=darkcyan>%memory% (%swap%) %topmem%</fc> \
      \| %disku% \
      \| <fc=magenta>%date%</fc> \
      \| %kbd% \
      \| <icon=mstate_%mstate%.xbm/>\
      \  <hspace=200/>"
  }

main :: IO ()
main = do
  datadir <- getDataDir
  xmobar $ config datadir
