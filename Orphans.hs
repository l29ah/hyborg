module Orphans where

import Data.Streaming.Process
import System.Exit (ExitCode (..))
import Control.Monad.IO.Unlift (MonadIO, liftIO, MonadUnliftIO, withUnliftIO, unliftIO)
import Data.Conduit
import Data.ByteString (ByteString)
import Control.Concurrent.Async (runConcurrently, Concurrently(..))
import Control.Exception (onException, finally)

import Data.Conduit.Process ()

-- |A copy of `sourceProcessWithStreams` that doesn't touch `stderr`
sourceProcessWithIOStreams
  :: MonadUnliftIO m
  => CreateProcess
  -> ConduitT () ByteString m () -- ^stdin
  -> ConduitT ByteString Void m a -- ^stdout
  -> m (ExitCode, a)
sourceProcessWithIOStreams cp producerStdin consumerStdout =
  withUnliftIO $ \u -> do
    (  (sinkStdin, closeStdin)
     , (sourceStdout, closeStdout)
     , UseProvidedHandle
     , sph) <- streamingProcess cp
    (_, resStdout) <-
      runConcurrently (
        (,)
        <$> Concurrently ((unliftIO u $ runConduit $ producerStdin .| sinkStdin) `finally` closeStdin)
        <*> Concurrently (unliftIO u $ runConduit $ sourceStdout .| consumerStdout))
      `finally` closeStdout
      `onException` terminateStreamingProcess sph
    ec <- waitForStreamingProcess sph
    return (ec, resStdout)

terminateStreamingProcess :: MonadIO m => StreamingProcessHandle -> m ()
terminateStreamingProcess = liftIO . terminateProcess . streamingProcessHandleRaw
