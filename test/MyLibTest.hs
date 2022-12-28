{-# LANGUAGE GHC2021 #-} 
module Main 
where 

import RIO 
import Data.STM.TimedBuffer 



-- This is just a very basic test to check the functionality. No proper thread
-- termination is implemented, thus the executable will fail. The main purpose 
-- is to check the timing requirements.
main :: IO() 
main = do 
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
