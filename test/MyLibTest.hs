{-# LANGUAGE GHC2021, OverloadedStrings #-} 
module Main 
where 

import RIO 
import qualified RIO.Text as T 
import qualified Data.Text.IO as T 

import Data.STM.TimedBuffer 

import Text.Show.Pretty 

import Conduit 

-- This is just a very basic test to check the functionality. No proper thread
-- termination is implemented, thus the executable will fail. The main purpose 
-- is to check the timing requirements.
main :: IO() 
main = do 
  test2 


test1 :: IO () 
test1 = do 
    buf <- newTimedBufferIO 5 

    void $ async $ do
        let action x = do 
                putStrLn "Writing value..."
                writeTimedBuffer buf x
                threadDelay 100_000
        putStrLn "Waiting 3 secs..."
        threadDelay 3_000_000
        mapM_ (action) [0 .. 20 :: Int]
        putStrLn "Thread terminates"

    putStrLn "Running loop"
    loop buf
  where
    loop buf = do
        vals <- readTimedBuffer (Timeout 2_000_000) buf 
        print vals
        loop buf


test2 :: IO ()
test2 = do 
  buf <- newTimedBufferIO 5 

  runConc $ conc (thread1 buf) <> conc (thread2 buf)

  exitSuccess


maxElems :: Int 
maxElems = 1000


thread1 :: TimedBuffer Int -> IO () 
thread1 buffer = do 
  runConduit $ enumFromToC 1 maxElems .| bufferWriter 
  where 
    bufferWriter = awaitForever $ \n -> writeTimedBuffer buffer n 


thread2 :: TimedBuffer Int -> IO () 
thread2 buffer = do 
  runConduit $ bufferSource .| showC .| takeCE maxElems .| sinkNull
  where 
    bufferSource = do
      vals <- readTimedBuffer (Timeout 2_000_000) buffer 
      yield vals 
      bufferSource 


showC :: ConduitT [Int] [Int] IO () 
showC = awaitForever $ \lst -> do 
  liftIO $ T.putStrLn (T.pack (ppShow lst))
  yield lst 