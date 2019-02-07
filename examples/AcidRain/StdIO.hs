{-# LANGUAGE FlexibleContexts #-}

module AcidRain.StdIO
    ( initConsole
    , printHealth
    , printError
    , userStream
    )
where

import Streamly
import Streamly.Prelude as S
import Control.Monad.IO.Class (liftIO)

initConsole :: IO ()
initConsole = return ()

printHealth :: MonadAsync m => String -> m ()
printHealth = liftIO . putStrLn

printError :: MonadAsync m => String -> m ()
printError = liftIO . putStrLn

userStream :: MonadAsync m => SerialT m String
userStream = S.repeatM (liftIO getLine)
