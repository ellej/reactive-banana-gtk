
module Reactive.Banana.Gtk.Event (
   signal0, signal1,
   handler0,
   handlerS0, handlerS1,
   event0, event0',
   eventS0, eventS1, eventS2,
   eventM0, eventM1,
   blockEvent
) where

import System.Glib.Signals
import System.Glib.GObject
import System.Glib.GError (failOnGError)

import Reactive.Banana
import Reactive.Banana.Frameworks

import Control.Monad.Trans.Reader (ReaderT, ask, runReaderT)
import Data.Typeable (Typeable)
import Control.Exception (Exception)
import Control.Exception.Lifted (throwIO, catch)

import Foreign.Ptr (Ptr)

-- Redefine the type synonym here so we can lose the gtk2/gtk3 dependency.
type EventM t = ReaderT (Ptr t) IO

-- Create a Signal (only needed if it doesn't exist yet)

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

-- Create an AddHandler from a Signal

handler :: (MonadIO m, GObjectClass object)
        => ((a -> m ()) -> callback) -- ^ A GTK event handler can receive multiple arguments, but the Reactive Banana
                                     -- handlers can't. This means the arguments have to be bundled or filtered.
        -> object                    -- ^ The widget.
        -> Signal object callback    -- ^ The signal.
        -> MomentIO (AddHandler a)   -- ^ The new 'AddHandler'.
handler bundleArgs widget sig = liftIO $ do
   (addHandler, runHandler) <- newAddHandler
   connectId <- on widget sig $ bundleArgs $ liftIO . runHandler
   return $ AddHandler $ \s -> do
      t <- register addHandler s
      return (t >> signalDisconnect connectId)

-- | Create a handler from a signal name for a signal with no parameters.
handler0 :: GObjectClass object
         => object                   -- ^ The Widget.
         -> SignalName               -- ^ The name of the signal.
         -> MomentIO (AddHandler ()) -- ^ The new 'AddHandler'.
handler0 widget name = handler id widget (signal0 name)

-- | Create a handler for a signal with no parameters.
handlerS0 :: (MonadIO m, GObjectClass object)
          => object                   -- ^ The widget.
          -> Signal object (m ())     -- ^ The signal.
          -> MomentIO (AddHandler ()) -- ^ The new 'AddHandler'.
handlerS0 = handler ($ ())

-- | Create a handler for a signal with one parameter.
handlerS1 :: (MonadIO m, GObjectClass object)
          => object                    -- ^ The widget.
          -> Signal object (a -> m ()) -- ^ The signal.
          -> MomentIO (AddHandler a)   -- ^ The new 'AddHandler'.
handlerS1 = handler id

-- Create an Event from an AddHandler

event :: (MonadIO m, GObjectClass object)
      => ((a -> m ()) -> callback) -- ^ Bundle or filter the arguments.
      -> object                    -- ^ The widget.
      -> Signal object callback    -- ^ The signal.
      -> MomentIO (Event a)        -- ^ The new 'Event'.
event bundleArgs widget sig = handler bundleArgs widget sig >>= fromAddHandler

event0 :: GObjectClass object
       => object                -- ^ The widget.
       -> SignalName            -- ^ The name of the signal.
       -> MomentIO (Event ())   -- ^ The new 'Event'.
event0 widget name = handler0 widget name >>= fromAddHandler

event0' :: GObjectClass object
        => object                          -- ^ The widget.
        -> SignalName                      -- ^ The name of the signal.
        -> (AddHandler () -> AddHandler b) -- ^ Optionally map the AddHandler.
        -> MomentIO (Event b) -- ^ The new 'Event'.
event0' widget name f = handler0 widget name >>= fromAddHandler . f

eventS0 :: (MonadIO m, GObjectClass object)
        => object                -- ^ The widget.
        -> Signal object (m ())  -- ^ The signal.
        -> MomentIO (Event ())   -- ^ The new 'Event'.
eventS0 = event ($ ())

eventS1 :: (MonadIO m, GObjectClass object)
        => object                    -- ^ The widget.
        -> Signal object (a -> m ()) -- ^ The signal.
        -> MomentIO (Event a)        -- ^ The new 'Event'.
eventS1 = event id

eventS2 :: (MonadIO m, GObjectClass object)
        => object                         -- ^ The widget.
        -> Signal object (a -> b -> m ()) -- ^ The signal.
        -> MomentIO (Event (a, b))        -- ^ The new 'Event'.
eventS2 = event curry

eventM0 :: GObjectClass object
        => object                        -- ^ The widget.
        -> Signal object (EventM a Bool) -- ^ The signal.
        -> MomentIO (Event ())           -- ^ The new 'Event'.
eventM0 = event (propEvent . ($ ()))

-- | EventM a Bool is a Reader containing a Ptr to the event value. Gtk2hs provides
-- functions that convert that pointer to the actual type (:: EventM a b).
eventM1 :: GObjectClass object
        => object                        -- ^ The widget.
        -> Signal object (EventM a Bool) -- ^ The signal.
        -> EventM a b                    -- ^ Get the event arguments stored in the Reader.
        -> MomentIO (Event b)            -- ^ The new 'Event'.
eventM1 widget sig value = event (propEvent . (value >>=)) widget sig

-- Handling exceptions.

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
