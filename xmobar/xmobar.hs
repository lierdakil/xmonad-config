module Main (main) where

import Data.Maybe
import Data.Text qualified as T
import Paths_xmonad_config
import System.Environment
import System.FilePath
import System.Process
import Xmobar
import Network.Socket (socket, connect, close, Family(AF_UNIX), SocketType(Stream), SockAddr(SockAddrUnix))
import Network.Socket.ByteString (recv)
import Data.Int (Int32)
import Data.Word ( Word16)
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as L
import Data.Binary.Get (Get, runGet, getInt32le, getWord8, getWord16le)
import Data.Functor ((<&>))
import Control.Applicative (many)
import Control.Exception (catch, SomeException)

data MState = MState String
  deriving stock (Show, Read)

parseInt32LE :: ByteString -> Int32
parseInt32LE bs = runGet getInt32le $ L.fromStrict bs

newtype KeyCode = KeyCode Word16
data KeyState
  = Released
  | Locked
  | WillRelease
  | Unknown
  deriving Eq

data MStateData = MStateData
  { scroll :: Bool
  , buttons :: [(KeyCode, KeyState)]
  }

parseMStateData :: Get MStateData
parseMStateData = MStateData <$> getBool <*> getButtons
  where
    getBool = (/= 0) <$> getWord8
    getButtons = many $ (,)
      <$> (KeyCode <$> getWord16le)
      <*> getKeyState
    getKeyState = getWord8 <&> \case
      0 -> Released
      1 -> Locked
      2 -> WillRelease
      _ -> Unknown

instance Exec MState where
    alias (MState _) = "mstate"
    rate (MState _) = 1
    run (MState path) = ignoreExceptions "xx" $ do
      sock <- socket AF_UNIX Stream 0
      connect sock (SockAddrUnix path)
      size <- fromIntegral . parseInt32LE <$> recv sock 4
      MStateData{..} <- runGet parseMStateData . L.fromStrict <$> recv sock size
      close sock
      secondParam <- case buttons of
        [] -> pure "0"
        xs -> do
          if any (\(_, st) -> st /= Released) xs
            then pure "2"
            else pure "1"
      pure $ show (fromEnum scroll) <> secondParam

ignoreExceptions :: a -> IO a -> IO a
ignoreExceptions def action = action `catch` \(_ :: SomeException) -> pure def

data PaMute = PaMute
  deriving stock (Read, Show)

instance Exec PaMute where
  alias _ = "mute"
  rate _ = 10
  run _ = do
    defaultEnv <- getEnvironment
    let env = Just $ ("LC_ALL", "C") : defaultEnv
    isMute <- T.pack <$> readCreateProcess (proc "pactl" ["get-sink-mute", "@DEFAULT_SINK@"]){env} ""
    volume <- T.pack <$> readCreateProcess (proc "pactl" ["get-sink-volume", "@DEFAULT_SINK@"]){env} ""
    let vol = fmap (T.justifyLeft 3 ' ' . T.strip) . listToMaybe $ drop 1 $ T.splitOn "/" volume
    pure $ case fmap T.strip $ T.stripPrefix "Mute:" isMute of
      Just "yes" -> " M "
      _ -> maybe "   " T.unpack vol

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
      , Run $ MState "/tmp/tweakpoint.sock"
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
