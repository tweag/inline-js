module Language.JavaScript.Inline.Core.IPC where

import Control.Concurrent
import Data.Binary.Get
import Data.ByteString.Builder
import qualified Data.ByteString.Lazy as LBS
import Foreign
import GHC.IO (catchAny)
import Language.JavaScript.Inline.Core.Utils
import System.IO

type Msg = LBS.ByteString

-- | An 'IPC' represents an opaque bi-directional message port. There are
-- middleware functions in this module which modify 'IPC' fields.
data IPC = IPC
  { -- | Send a 'Msg'.
    send :: Msg -> IO (),
    -- | Receive a 'Msg'. Should block when there's no incoming 'Msg' yet, and
    -- throw when the 'IPC' is closed.
    recv :: IO Msg,
    -- | Callback for each incoming 'Msg'.
    onRecv :: Msg -> IO (),
    -- | The 'Msg' to notify the remote device to shutdown. No more 'Msg's may
    -- be sent after this is sent, but more incoming 'Msg's are still permitted
    -- (until 'recv' throws). This allows graceful shutdown behavior.
    closeMsg :: Msg,
    -- | The callback to be called when 'closeMsg' is sent.
    preClose :: IO (),
    -- | The callback to be called when 'recv' throws, which indicates the
    -- remote device has closed.
    postClose :: IO ()
  }

instance Show IPC where
  show IPC {} = "IPC"

-- | Given the 'Handle's for send/recv, this function adds the
-- 'send'/'recv'/'preClose' fields to an 'IPC' value.
--
-- The protocol for preserving message boundaries is simple: first comes the
-- message header, which is just a little-endian 64-bit unsigned integer,
-- representing the message byte length (sans header). Then follows the actual
-- message.

-- The 'Handle' for send is flushed after each 'send' call to allow the remote
-- device to get the 'Msg' immediately.
ipcFromHandles :: Handle -> Handle -> IPC -> IPC
ipcFromHandles h_send h_recv ipc =
  ipc
    { send = \msg -> do
        hPutBuilder h_send $
          storablePut (LBS.length msg)
            <> lazyByteString msg
        hFlush h_send,
      recv = do
        len <- runGet storableGet <$> hGetExact h_recv 8
        hGetExact h_recv $ fromIntegral (len :: Word64),
      preClose = hClose h_send
    }

-- | This function forks the send/recv threads. In the result 'IPC' value, only
-- the 'send'/'closeMsg' fields remain valid and can be used by the user.
--
-- The send thread repeatedly fetches 'Msg's from a 'Channel' and send to the
-- remote device; when the 'closeMsg' message is sent, it invokes the 'preClose'
-- callback and exits. The 'send' function in the result 'IPC' is thus
-- asynchronous; it'll enqueue a 'Msg' and immediately return. We don't bother
-- to track when the 'Msg' is actually sent by the original 'send' function.
--
-- The recv thread repeatedly fetches incoming 'Msg's and invokes the 'onRecv'
-- callback on them. When an exception is raised, it invokes the 'postClose'
-- callback and exits. Since 'onRecv' is run in the recv thread, the user should
-- ensure it doesn't throw and doesn't take too long to complete, otherwise
-- it'll block later incoming messages.
--
-- Performing send/recv in worker threads enables thread safety for 'IPC' end
-- users.
ipcFork :: IPC -> IO IPC
ipcFork ipc = do
  chan_send <- newChan
  let io_send = do
        msg <- readChan chan_send
        send ipc msg
        if msg == closeMsg ipc then preClose ipc else io_send
  _ <- forkIO io_send
  let io_recv_loop = do
        msg <- recv ipc
        onRecv ipc msg
        io_recv_loop
      io_recv = catchAny io_recv_loop $ \_ -> postClose ipc
  _ <- forkIO io_recv
  pure
    ipc
      { send = writeChan chan_send,
        recv = error "fork: recv",
        onRecv = error "fork: onRecv",
        preClose = error "fork: preClose",
        postClose = error "fork: postClose"
      }
