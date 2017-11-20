module Lib
    ( someFunc
    ) where

import Control.Concurrent (threadDelay, forkIO)
import Control.Monad (forever)
import Data.Time.LocalTime
import Pipes as P
import Pipes.Concurrent as P
import qualified Pipes.Prelude as P
import System.Process

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
  let tot = read . head . drop 1 . words . head . lines $ result
      free = read . head . drop 1 . words . head . drop 1 . lines $ result
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

watchProducer :: (Show a) => Producer a IO () -> Output String -> IO ()
watchProducer p output = do
  _ <- forkIO $ do
    runEffect $ p >-> P.show >-> unique "" >-> toOutput output
    performGC
  return ()

toBar :: Double -> String
toBar x = "^r(3x" ++ show (round (x*10)) ++ ")"

watchBar :: Producer Double IO () -> Output String -> IO ()
watchBar p output = do
  _ <- forkIO $ do
    runEffect $ p >-> unique 0 >-> P.map toBar >-> toOutput output
    performGC
  return ()

someFunc :: IO ()
someFunc = do
  (output, input) <- spawn unbounded

  watchProducer clockTime output
  watchBar disks output
  watchProducer mail output
  watchBar cpu output
  watchBar mem output

  runEffect $ fromInput input >-> P.stdoutLn
