module Main (main) where

import Control.Monad
import Data.Bifunctor
import Data.IntMap (IntMap)
import Data.IntMap qualified as IntMap
import Data.Maybe
import Data.Text qualified as T
import Paths_xmonad_config
import System.Environment
import System.FilePath
import System.Process
import Text.Read
import Xmobar

data MState = MState
  deriving stock (Show, Read)

instance Exec MState where
    alias MState = "mstate"
    rate MState = 1
    run MState = do
      mouseName <- fromMaybe "Virtual core pointer" <$> lookupEnv "MOUSE"
      XInputData{..} <- parseXInput . T.pack <$> readProcess "xinput" ["list-props", mouseName] ""
      secondParam <- case xidEvdevDragLockButtons of
        Nothing -> pure "0"
        Just xs -> do
          XInputState{..} <- parseXInputState . T.pack <$> readProcess "xinput" ["query-state", mouseName] ""
          if any (\idx -> IntMap.lookup idx xisButtons == Just True) xs
            then pure "2"
            else pure "1"
      pure $ show (fromEnum xidEvdevWheelEmulation) <> secondParam

data XInputData = XInputData
  { xidEvdevWheelEmulation :: Bool
  , xidEvdevDragLockButtons :: Maybe [Int]
  }

parseXInput :: T.Text -> XInputData
parseXInput = foldr go defXInputData . map prepare . T.lines where
  defXInputData = XInputData False Nothing
  prepare = join bimap T.strip . second (T.drop 1) . T.breakOn ":"
  go (k, v) acc = case k of
    "Evdev Wheel Emulation (318)" ->
      acc{xidEvdevWheelEmulation = parseBool v}
    "Evdev Drag Lock Buttons (323)" ->
      acc{xidEvdevDragLockButtons = parseMbIntList v}
    _ -> acc
  parseBool = \case
    "1" -> True
    _ -> False
  parseMbIntList = \case
    "0" -> Nothing
    x -> Just . mapMaybe (readMaybe . T.unpack . T.strip) . T.splitOn "," $ x

data XInputState = XInputState
  { xisButtons :: IntMap Bool
  }

parseXInputState :: T.Text -> XInputState
parseXInputState = foldr go defXInputState . map prepare . T.lines where
  defXInputState = XInputState mempty
  prepare = join bimap T.strip . second (T.drop 1) . T.breakOn "="
  go (k, v) acc@XInputState{..}
    | Just idx <- readMaybe . T.unpack =<< T.stripSuffix "]" =<< T.stripPrefix "button[" k
    = acc { xisButtons = IntMap.insert idx (parseBtnState v) xisButtons }
    | otherwise
    = acc
  parseBtnState = \case
    "down" -> True
    _ -> False

data PaMute = PaMute
  deriving stock (Read, Show)

instance Exec PaMute where
  alias _ = "mute"
  rate _ = 1
  run _ = do
    defaultEnv <- getEnvironment
    let env = Just $ ("LC_ALL", "C") : defaultEnv
    isMute <- T.pack <$> readCreateProcess (proc "pactl" ["get-sink-mute", "@DEFAULT_SINK@"]){env} ""
    pure $ case fmap T.strip $ T.stripPrefix "Mute:" isMute of
      Just "yes" -> "M"
      _ -> " "

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
      , Run $ MState -- Com "xinput-mouse-state" [] "mstate" 1
      , Run $ PaMute
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
      \%mute%\
      \  <hspace=200/>"
  }

main :: IO ()
main = do
  datadir <- getDataDir
  xmobar $ config datadir
