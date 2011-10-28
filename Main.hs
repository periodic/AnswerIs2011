module Main where

import Answer
import System.Environment
import Control.Concurrent
import GHC.Conc (numCapabilities)
import Control.Monad
import System.IO


{- | The list of expressions to check.
 -}
dataSource = enumerate 4 3

{- | Size of chunks per thread.
 -}
chunkSize = 4096

main = do
    [targArg] <- getArgs

    let target = read targArg :: Double

    parMain target

{- | An attempt at an overly complicated multi-threaded solution.
 -}
parMain target = do
    putStrLn $ "Preparing to run on " ++ show numCapabilities ++ " cores."

    dataVar <- newEmptyMVar   :: IO (MVar [Expression])
    outputVar <- newEmptyMVar :: IO (MVar (Maybe Expression))
    threadVars <- replicateM numCapabilities newEmptyMVar :: IO [MVar ()]

    processThreads <- mapM (forkIO . processor target outputVar dataVar) threadVars
    printerThread  <- forkIO $ printer outputVar
    providerThread <- forkIO $ provider dataVar dataSource

    mapM_ takeMVar threadVars
    putMVar outputVar Nothing

    where
        provider :: MVar [Expression] -> [Expression] -> IO ()
        provider dataVar list = do
            let (chunk, rest) = splitAt chunkSize list
            putMVar dataVar chunk
            unless (chunk == []) $ provider dataVar rest

        printer :: MVar (Maybe Expression) -> IO ()
        printer outputVar = do
            mExp <- takeMVar outputVar 
            case mExp of
                Just e  -> print e >> printer outputVar
                Nothing -> return ()

        processor :: Double -> MVar (Maybe Expression) -> MVar [Expression] -> MVar () -> IO ()
        processor target outputVar dataVar threadVar = do
            datums <- takeMVar dataVar
            case datums of
                [] -> putMVar dataVar [] >> putMVar threadVar ()
                ds -> do
                    mapM (putMVar outputVar. Just) . filter (maybe False (==target) . eval) $ ds
                    putStr "."
                    hFlush stdout
                    processor target outputVar dataVar threadVar




