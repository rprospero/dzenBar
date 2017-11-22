{-# LANGUAGE Rank2Types, OverloadedStrings #-}
module Lib
    ( someFunc
    ) where

import Control.Concurrent (threadDelay, forkIO)
import Control.Lens as L
import Control.Monad (forever)
import Data.Time.Calendar
import Data.Time.Format
import Data.Time.LocalTime
import Graphics.Icons.FontAwesome
import Graphics.Icons.Types
import Pipes as P
import Pipes.Concurrent as P
import qualified Pipes.Prelude as P
import System.IO
import System.Process

data Bar = Bar {
  _clock :: ZonedTime,
  _disks :: Double,
  _mail :: Int,
  _cpu :: Double,
  _mem :: Double,
  _status :: String}

defaultBar :: Bar
defaultBar = Bar {
  _clock = ZonedTime (LocalTime (ModifiedJulianDay 0) midnight) utc,
  _disks = 0,
  _mail = 0,
  _cpu = 0,
  _mem = 0,
  _status = ""}

instance Show Bar where
  show x = "^tw()" ++ (formatTime defaultTimeLocale "%R" $ _clock x) ++ " "
    ++ _status x ++ "^bg() "
    ++ iconDzen hddOCode ++ toBar (_disks x) ++ " "
    ++ iconDzen envelopeCode ++ show (_mail x) ++ " "
    ++ toBar (_cpu x) ++ " "
    ++ toBar (_mem x)

__clock :: Lens' Bar ZonedTime
__clock = lens _clock (\ bar x -> bar{_clock=x})
__disks :: Lens' Bar Double
__disks = lens _disks (\ bar x -> bar{_disks=x})
__mail :: Lens' Bar Int
__mail = lens _mail (\ bar x -> bar{_mail=x})
__cpu :: Lens' Bar Double
__cpu = lens _cpu (\ bar x -> bar{_cpu=x})
__mem :: Lens' Bar Double
__mem = lens _mem (\ bar x -> bar{_mem=x})
__status :: Lens' Bar String
__status = lens _status (\ bar x -> bar{_status=x})

clockTime ::Producer ZonedTime IO r
clockTime = forever $ do
  lift getZonedTime >>= yield
  lift $ threadDelay 2000000

disks :: Producer Double IO r
disks = forever $ do
  results <- lift $ readProcess "/usr/bin/df" [] ""
  yield . read . init . head . drop 4 . words . head . drop 1 . lines $ results
  lift $ threadDelay 6000000

mail :: Producer Int IO r
mail = forever $ do
  result <- lift $ readProcess "/usr/bin/notmuch" ["count", "tag:unread"] ""
  yield $ read result
  lift $ threadDelay 5000000

cpu :: Producer Double IO r
cpu = forever $ do
  result <- lift $ readFile "/proc/loadavg"
  yield . read . head.words.head.lines $ result
  lift $ threadDelay 5000000

mem :: Producer Double IO r
mem = forever $ do
  result <- lift $ readFile "/proc/meminfo"
  let tot = read . (!! 1) . words . head . lines $ result
      free = read . (!! 1) . words . (!! 1) . lines $ result
  yield $ (tot-free)/tot
  lift $ threadDelay 5000000

unique :: (Monad m, Eq x) => x -> Pipe x x m r
unique state = do
  new <- await
  if new /= state
    then do
    yield new
    unique new
    else unique new

apply :: (Monad m) => x -> Pipe (x -> x) x m r
apply x = do
  result <- await
  let new = result x
  yield new
  apply new

watchProducer :: (Show a) => Producer a IO () -> Lens' Bar a -> Output (Bar -> Bar) -> IO ()
watchProducer p lens output = do
  _ <- forkIO $ do
    runEffect $ p >-> P.map (\x bar -> bar & lens .~ x) >-> toOutput output
    performGC
  return ()

toBar :: Double -> String
toBar x = "^r(3x" ++ show (round (x*10)) ++ ")"

someFunc :: IO ()
someFunc = do
  hSetBuffering stdout LineBuffering
  (output, input) <- spawn unbounded

  watchProducer clockTime __clock output
  watchProducer disks __disks output
  watchProducer mail __mail output
  watchProducer cpu __cpu output
  watchProducer mem __mem output
  watchProducer P.stdinLn __status output

  runEffect $ fromInput input >-> apply defaultBar >-> P.show >-> unique "" >-> P.stdoutLn
