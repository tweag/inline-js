-- Original implementation: https://github.com/fpco/typed-process/pull/12

module Language.JavaScript.Inline.Core.WaitForProcess where

import Control.Concurrent
import GHC.RTS.Flags
import System.Exit
import System.Process hiding (waitForProcess)
import qualified System.Process as P

waitForProcess :: ProcessHandle -> IO ExitCode
waitForProcess ph
  | rtsSupportsBoundThreads = P.waitForProcess ph
  | otherwise = do
      switchTime <- fromIntegral . (`div` 1000) . ctxtSwitchTime <$> getConcFlags
      let minDelay = 1
          maxDelay = max minDelay switchTime
          loop delay = do
            threadDelay delay
            mec <- getProcessExitCode ph
            case mec of
              Nothing -> loop $ min maxDelay (delay * 2)
              Just ec -> pure ec
      loop minDelay
