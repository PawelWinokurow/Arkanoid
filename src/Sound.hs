module Sound (module Sound) where

import Control.Monad
import Control.Concurrent
import Sound.ProteaAudio

playSound :: Sample -> MVar Bool -> Int -> IO ()
playSound sample mvar i = do
  takeMVar mvar
  soundPlay sample 1 1 0 1 
  waitUntilEnd
  playSound sample mvar i

waitUntilEnd :: IO ()
waitUntilEnd = do
  n <- soundActive
  when  (n > 0) $ do
    threadDelay 10000
    waitUntilEnd