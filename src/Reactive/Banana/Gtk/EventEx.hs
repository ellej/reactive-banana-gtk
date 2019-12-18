
module Reactive.Banana.Gtk.EventEx (
   handlerS0', handlerS1',
   eventS0', eventS1', eventS2',
   eventM0', eventM1'
) where

import System.Glib.Signals
import System.Glib.GObject
import Reactive.Banana
import Reactive.Banana.Frameworks
import Control.Exception.Lifted (finally)

import Reactive.Banana.Gtk.Shared

handler' :: (MonadIO m, GObjectClass object)
        => ((a -> m ()) -> callback)               -- ^ A GTK event handler can receive multiple arguments,
                                                   -- but the Reactive Banana handlers can't. This means the
                                                   -- arguments have to be bundled or filtered.
        -> object                                  -- ^ The widget.
        -> Signal object callback                  -- ^ The signal.
        -> MomentIO (AddHandler a, IO x -> IO x)   -- ^ The new 'AddHandler' and a way to execute an action
                                                   -- without without triggering this signal.
handler' bundleArgs widget sig = liftIO $ do
   (addHandler, runHandler) <- newAddHandler
   connectId <- on widget sig $ bundleArgs $ liftIO . runHandler
   let doQuietly action = signalBlock connectId >> finally action (signalUnblock connectId)
   return (AddHandler $ \s -> do
      t <- register addHandler s
      return (t >> signalDisconnect connectId), doQuietly)

-- | Create a handler for a signal with no parameters.
handlerS0' :: (MonadIO m, GObjectClass object)
          => object                                   -- ^ The widget.
          -> Signal object (m ())                     -- ^ The signal.
          -> MomentIO (AddHandler (), IO x -> IO x)   -- ^ The new 'AddHandler'.
handlerS0' = handler' ($ ())

-- | Create a handler for a signal with one parameter.
handlerS1' :: (MonadIO m, GObjectClass object)
          => object                                -- ^ The widget.
          -> Signal object (a -> m ())             -- ^ The signal.
          -> MomentIO (AddHandler a, IO x -> IO x) -- ^ The new 'AddHandler'.
handlerS1' = handler' id

event' :: (MonadIO m, GObjectClass object)
      => ((a -> m ()) -> callback)           -- ^ Bundle or filter the arguments.
      -> object                              -- ^ The widget.
      -> Signal object callback              -- ^ The signal.
      -> MomentIO (Event a, IO x -> IO x)    -- ^ The new 'Event'.
event' bundleArgs widget sig = do
   (ah, doQuietly) <- handler' bundleArgs widget sig
   e <- fromAddHandler ah
   return (e, doQuietly)

eventS0' :: (MonadIO m, GObjectClass object)
        => object                               -- ^ The widget.
        -> Signal object (m ())                 -- ^ The signal.
        -> MomentIO (Event (), IO x -> IO x)    -- ^ The new 'Event'.
eventS0' = event' ($ ())

eventS1' :: (MonadIO m, GObjectClass object)
        => object                            -- ^ The widget.
        -> Signal object (a -> m ())         -- ^ The signal.
        -> MomentIO (Event a, IO x -> IO x)  -- ^ The new 'Event'.
eventS1' = event' id

eventS2' :: (MonadIO m, GObjectClass object)
        => object                                  -- ^ The widget.
        -> Signal object (a -> b -> m ())          -- ^ The signal.
        -> MomentIO (Event (a, b), IO x -> IO x)   -- ^ The new 'Event'.
eventS2' = event' curry

eventM0' :: GObjectClass object
        => object                            -- ^ The widget.
        -> Signal object (EventM a Bool)     -- ^ The signal.
        -> MomentIO (Event (), IO x -> IO x) -- ^ The new 'Event'.
eventM0' = event' (propEvent . ($ ()))

-- | EventM a Bool is a Reader containing a Ptr to the event value. Gtk2hs provides
-- functions that convert that pointer to the actual type (:: EventM a b).
eventM1' :: GObjectClass object
        => object                            -- ^ The widget.
        -> Signal object (EventM a Bool)     -- ^ The signal.
        -> EventM a b                        -- ^ Get the event arguments stored in the Reader.
        -> MomentIO (Event b, IO x -> IO x)  -- ^ The new 'Event'.
eventM1' widget sig value = event' (propEvent . (value >>=)) widget sig
