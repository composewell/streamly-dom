{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}

-- This example is adapted from Gabriel Gonzalez's pipes-concurrency package.
-- https://hackage.haskell.org/package/pipes-concurrency-2.0.8/docs/Pipes-Concurrent-Tutorial.html

module Main
    ( main
    ) where

import Streamly
import Streamly.Prelude as S
import Control.Monad (void)
import Control.Monad.State (MonadState, get, modify, runStateT)

#if __GHCJS__
import AcidRain.JSIO
#else
import AcidRain.StdIO
#endif

-------------------------------------------------------------------------------
-- Streams
-------------------------------------------------------------------------------

data GEvent = Quit | Harm Int | Heal Int | Unknown deriving (Show)

parseCommand :: String -> GEvent
parseCommand command =
    case command of
        "potion" -> Heal 10
        "harm"   -> Harm 10
        "quit"   -> Quit
        _        -> Unknown

acidRain :: MonadAsync m => SerialT m GEvent
acidRain = asyncly $ constRate 1 $ S.repeatM $ return $ Harm 1

data Result = Check | Done | Error

runEvents :: (MonadState Int m, MonadAsync m) => SerialT m Result
runEvents = do
    event <- fmap parseCommand userStream `parallel` acidRain
    case event of
        Harm n -> modify (\h -> h - n) >> return Check
        Heal n -> modify (\h -> h + n) >> return Check
        Unknown -> return Error
        Quit -> return Done

data Status = Alive | GameOver deriving Eq

getStatus :: (MonadState Int m, MonadAsync m) => Result -> m Status
getStatus result = do
    case result of
        Done -> printHealth "You quit!" >> return GameOver
        Error -> printError "Type potion or harm or quit" >> return Alive
        Check -> do
            health <- get
            if (health <= 0)
                then printHealth "You die!" >> return GameOver
                else printHealth ("Health = " <> show health) >>
                     return Alive

-------------------------------------------------------------------------------
-- Game
-------------------------------------------------------------------------------

letItRain :: IO ()
letItRain = do
    initConsole
    let runGame =
            S.drainWhile (== Alive) $ S.mapM getStatus runEvents
    void $ runStateT runGame 60

main :: IO ()
main = letItRain
