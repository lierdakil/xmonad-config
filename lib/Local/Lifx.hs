{-# LANGUAGE OverloadedStrings, ConstraintKinds #-}
module Local.Lifx (
  command
, withDuration
, PowerState (On, Off)
, powerState
-- location specifiers
, group
, label
, identifier
, groupId
, locationId
, location
, random
-- color specifiers
, addColor
, hue
, saturation
, brightness
, kelvin
, rgb
--
, ColorSpec( White
           , Red
           , Orange
           , Yellow
           , Cyan
           , Green
           , Blue
           , Purple
           , Pink)
--
, def
, Lifx
, LifxCommand
) where

import           Data.Aeson                 ((.=))
import qualified Data.Aeson                 as JSON
import qualified Data.ByteString.Char8      as B
import qualified Data.ByteString.Lazy.Char8 as LB
import           Data.Default
import           Data.List                  (intercalate)
import           Data.String
import           Network.Connection         (TLSSettings (..))
import           Network.HTTP.Conduit
import           Data.Monoid ((<>))

class DurationAction a where
  withDuration :: Float -> a -> a

type Lifx a = (JSON.ToJSON a, LifxAction a)

data LifxCommand a = LifxCommand { lcAction   :: a
                                 , lcSelector :: LifxSelector
                                 }

instance LifxAction a => Default (LifxCommand a) where
  def = LifxCommand{lcAction=defAct, lcSelector = All}

class LifxAction a where
  cmdURI    :: a -> String
  cmdMethod :: a -> B.ByteString
  defAct    :: a

instance LifxAction PowerStateAction where
  cmdURI _    = "state"
  cmdMethod _ = "PUT"
  defAct      = PowerStateAction Nothing On

instance LifxAction ColorAction where
  cmdURI _    = "state"
  cmdMethod _ = "PUT"
  defAct      = ColorAction Nothing (Color [])

data PowerStateAction = PowerStateAction { psDuration   :: Maybe Float
                                         , psPowerState :: PowerState
                                         }

type PowerStateCommand = LifxCommand PowerStateAction

powerState :: PowerState -> PowerStateCommand -> PowerStateCommand
powerState s c = c{lcAction = PowerStateAction Nothing s}

instance DurationAction PowerStateAction where
  withDuration duration a = a{psDuration=Just duration}

data PowerState = On | Off

data ColorAction = ColorAction { ccDuration :: Maybe Float
                               , ccColor    :: Color
                               }

type ColorCommand = LifxCommand ColorAction

instance DurationAction ColorAction where
  withDuration duration a = a{ccDuration=Just duration}

newtype Color = Color [ColorSpec]

data ColorSpec =   Hue Int
                 | Saturation Float
                 | Brightness Float
                 | Kelvin Int
                 | White
                 | Red
                 | Orange
                 | Yellow
                 | Cyan
                 | Green
                 | Blue
                 | Purple
                 | Pink
                 | RGB Int Int Int
                --  | ColorHash String

instance Semigroup Color where
  (Color a) <> (Color b) = Color (a++b)

instance Monoid Color where
  mempty = Color []

addColor :: Color -> ColorCommand -> ColorCommand
addColor c cmd@LifxCommand{lcAction=(a@ColorAction{ccColor=cs})} =
  cmd{lcAction= a{ccColor=cs <> c}}

limit :: (Ord a, Num a) => a -> a -> a
limit l a | a<0        = 0
          | a>l        = l
          | otherwise = a

hue :: Int -> ColorCommand -> ColorCommand
hue = addColor . Color . return . Hue . limit 360

saturation :: Float -> ColorCommand -> ColorCommand
saturation = addColor . Color . return . Saturation . limit 1

brightness :: Float -> ColorCommand -> ColorCommand
brightness = addColor . Color . return . Brightness . limit 1

kelvin :: Int -> ColorCommand -> ColorCommand
kelvin = addColor . Color . return . Kelvin . limit 9000

rgb :: Int -> Int -> Int -> ColorCommand -> ColorCommand
rgb = (((addColor . Color . return) .) .) . RGB

instance JSON.ToJSON PowerState where
  toJSON On  = JSON.String "on"
  toJSON Off = JSON.String "off"

instance JSON.ToJSON Color where
  toJSON (Color s) = JSON.String $ fromString $
    unwords $ map toStr s
    where
      toStr (Hue i)        = "hue:"++show i
      toStr (Saturation i) = "saturation:"++show i
      toStr (Brightness i) = "brightness:"++show i
      toStr (Kelvin i)     = "kelvin:"++show i
      toStr White   = "white"
      toStr Red     = "red"
      toStr Orange  = "orange"
      toStr Yellow  = "yellow"
      toStr Cyan    = "cyan"
      toStr Green   = "green"
      toStr Blue    = "blue"
      toStr Purple  = "purple"
      toStr Pink    = "pink"
      toStr (RGB r g b) = "rgb:"++intercalate "," (map show [r,g,b])
      -- toStr (ColorHash s) = s

instance JSON.ToJSON PowerStateAction where
  toJSON PowerStateAction{psPowerState=s,psDuration=d} =
    JSON.object ("power" .= s : duration)
    where
      duration = maybe [] (return . ("duration" .=)) d

instance JSON.ToJSON ColorAction where
  toJSON ColorAction{ccColor=c,ccDuration=d} =
    JSON.object ( "color"    .= c : duration )
    where
      duration = maybe [] (return . ("duration" .=)) d


data LifxSelector =   All
                    | Label String
                    | Id String
                    | GroupId String
                    | Group String
                    | LocId String
                    | Loc String
                    | Random

group :: LifxAction a => String -> LifxCommand a -> LifxCommand a
group g c = c{lcSelector = Group g}

label :: LifxAction a => String -> LifxCommand a -> LifxCommand a
label l c = c{lcSelector = Label l}

identifier :: LifxAction a => String -> LifxCommand a -> LifxCommand a
identifier i c = c{lcSelector=Id i}

groupId :: LifxAction a => String -> LifxCommand a -> LifxCommand a
groupId i c = c{lcSelector=GroupId i}

locationId :: LifxAction a => String -> LifxCommand a -> LifxCommand a
locationId i c = c{lcSelector=LocId i}

location :: LifxAction a => String -> LifxCommand a -> LifxCommand a
location i c = c{lcSelector=Loc i}

random :: LifxAction a => LifxCommand a -> LifxCommand a
random c = c{lcSelector=Random}

selectorToString :: LifxSelector -> String
selectorToString All = "all"
selectorToString Random = "random"
selectorToString (Label s) = "label:"++s
selectorToString (Id s) = "id:"++s
selectorToString (GroupId s) = "group_id:"++s
selectorToString (Group s) = "group:"++s
selectorToString (LocId s) = "location_id:"++s
selectorToString (Loc s) = "location:"++s

commandURI :: LifxAction a => LifxCommand a -> (String,B.ByteString)
commandURI LifxCommand{lcSelector=sel,lcAction=c} =
  ("/lights/"++selectorToString sel++"/"++cmdURI c++".json",cmdMethod c)

sendRequest :: String -> (String, B.ByteString) -> LB.ByteString -> IO String
sendRequest token (uri,method') body = do
  let lifxapi = "https://api.lifx.com/v1"
  request <- parseRequest $ lifxapi++uri
  let req = request{method=method', requestHeaders=headers', requestBody=RequestBodyBS $ LB.toStrict body}
      settings = mkManagerSettings (TLSSettingsSimple True False False) Nothing
      headers' = [("Authorization",B.pack $ "Bearer "++token)]
  manager <- newManager settings
  resp <- httpLbs req manager
  return $ LB.unpack $ responseBody resp

command :: (LifxAction a, JSON.ToJSON a) => String -> (LifxCommand a -> LifxCommand a) -> IO String
command token cb =
  sendRequest token (commandURI c) $ JSON.encode $ lcAction c
  where c = cb def
