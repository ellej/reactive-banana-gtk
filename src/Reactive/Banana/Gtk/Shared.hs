
module Reactive.Banana.Gtk.Shared where

import System.Glib.Signals
import System.Glib.GObject
import System.Glib.GError (failOnGError)
import Reactive.Banana.Frameworks
import Data.Typeable (Typeable)
import Control.Exception (Exception)
import Control.Exception.Lifted (throwIO, catch)
import Control.Monad.Trans.Reader (ReaderT, ask, runReaderT)
import Foreign.Ptr (Ptr)

-- Redefine the type synonym here so the gtk2/gtk3 dependency is unnessecary.
type EventM t = ReaderT (Ptr t) IO

signal0 :: GObjectClass object
        => SignalName                 -- ^ The name of the signal.
        -> Signal object (() -> IO a) -- ^ The new 'Signal'.
signal0 name = Signal $ \connectAfter object handler' ->
   connectGeneric name connectAfter object (\_ -> failOnGError $ handler' ())

signal1 :: GObjectClass object
        => SignalName                -- ^ The name of the signal.
        -> (s -> t)                  -- ^ Transform the orignal argument to something we can use.
        -> Signal object (t -> IO a) -- ^ The new 'Signal'.
signal1 name f = Signal $ \connectAfter object handler' ->
   connectGeneric name connectAfter object (\_ p1 -> failOnGError $ handler' $ f p1)

data BlockEvent = BlockEvent deriving (Show, Typeable)
instance Exception BlockEvent

-- | Try to handle an event. The event is propagated by default, unless a BlockEvent exception is thrown.
-- See 'blockEvent'.
propEvent :: EventM any () -> EventM any Bool
propEvent act = do
   ptr <- ask
   liftIO (runReaderT (False <$ act) ptr) `catch` \BlockEvent -> return True

-- | Stop the current event from propagating.
blockEvent :: MonadIO m => m a
blockEvent = liftIO $ throwIO BlockEvent
