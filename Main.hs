module Main where

import Answer
import System.Environment
import Control.Concurrent
import GHC.Conc (numCapabilities)


{- | The list of expressions to check.
 -}
dataSource = enumerate 3 4

main = do
    [targArg] <- getArgs

    let target = read targArg :: Double

    parMain target

{- | An attempt at an overly complicated multi-threaded solution.
 -}
parMain target = do

    dataVar <- newEmptyMVar   :: IO (MVar [Expression])
    outputVar <- newEmptyMVar :: IO (MVar Expression)
    doneVar <- newEmptyMVar   :: IO (MVar ())

    putStrLn $ "Preparing to run on " ++ show numCapabilities ++ " cores."

    processThreads <- sequence . replicate numCapabilities . forkIO $ processor outputVar dataVar target
    printerThread  <- forkIO $ printer outputVar
    providerThread <- forkIO $ provider dataVar doneVar dataSource

    takeMVar doneVar

    where
        provider :: MVar [Expression] -> MVar () -> [Expression] -> IO ()
        provider dataVar doneVar list = do
            let (chunk, rest) = splitAt 1024 list
            case chunk of
                [] -> putMVar doneVar ()
                ds -> putMVar dataVar ds >> provider dataVar doneVar rest

        printer :: MVar Expression -> IO ()
        printer outputVar = takeMVar outputVar >>= print >> printer outputVar

        processor :: MVar Expression -> MVar [Expression] -> Double -> IO ()
        processor outputVar dataVar target = do
            datums <- takeMVar dataVar
            sequence . map (putMVar outputVar) . filter (maybe False (==target) . eval) $ datums
            processor outputVar dataVar target




