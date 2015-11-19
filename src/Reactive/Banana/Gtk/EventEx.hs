
{-# LANGUAGE RecursiveDo, TupleSections #-}

module Reactive.Banana.Gtk.EventEx (
   handlerS0
) where

import System.Glib.Signals
import System.Glib.GObject
import System.Glib.GError (failOnGError)

import Reactive.Banana
import Reactive.Banana.Frameworks

import Control.Monad.Trans.Reader (ReaderT, ask, runReaderT)
import Data.Typeable (Typeable)
import Control.Exception (Exception)
import Control.Exception.Lifted (throwIO, catch, finally)

import Foreign.Ptr (Ptr)

handler :: (MonadIO m, GObjectClass object)
        => ((a -> m ()) -> callback)               -- ^ A GTK event handler can receive multiple arguments,
                                                   -- but the Reactive Banana handlers can't. This means the
                                                   -- arguments have to be bundled or filtered.
        -> object                                  -- ^ The widget.
        -> Signal object callback                  -- ^ The signal.
        -> MomentIO (AddHandler a, IO x -> IO x)   -- ^ The new 'AddHandler' and a way to execute an action
                                                   -- without without triggering this signal.
handler bundleArgs widget sig = liftIO $ mdo
   (addHandler, runHandler) <- newAddHandler
   connectId <- on widget sig $ bundleArgs $ liftIO . runHandler
   let doQuietly action = signalBlock connectId >> finally action (signalUnblock connectId)
   return (AddHandler $ \s -> do
      t <- register addHandler s
      return (t >> signalDisconnect connectId), doQuietly)

-- | Create a handler for a signal with no parameters.
handlerS0 :: (MonadIO m, GObjectClass object)
          => object                   -- ^ The widget.
          -> Signal object (m ())     -- ^ The signal.
          -> MomentIO (AddHandler (), IO x -> IO x) -- ^ The new 'AddHandler'.
handlerS0 = handler ($ ())