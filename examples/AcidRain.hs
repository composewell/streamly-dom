{-# LANGUAGE FlexibleContexts #-}

-- This example is adapted from Gabriel Gonzalez's pipes-concurrency package.
-- https://hackage.haskell.org/package/pipes-concurrency-2.0.8/docs/Pipes-Concurrent-Tutorial.html

module Main
    ( main
    ) where

import Streamly
import Streamly.Prelude as S
import Control.Concurrent
       (MVar, newEmptyMVar, putMVar, takeMVar, threadDelay)
import Control.Monad (void, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State (StateT, MonadState, get, modify, runStateT, put)
import Control.Monad.Reader (ask)

import GHCJS.DOM
import GHCJS.DOM.Types
import GHCJS.DOM.Window (promptUnchecked)
import GHCJS.DOM.KeyboardEvent
import GHCJS.DOM.Document
import GHCJS.DOM.Element
import GHCJS.DOM.HTMLInputElement
import GHCJS.DOM.NonElementParentNode
import GHCJS.DOM.Node
import GHCJS.DOM.EventM
import GHCJS.DOM.GlobalEventHandlers
import GHCJS.DOM.HTMLHyperlinkElementUtils

data GEvent = Quit | Harm Int | Heal Int deriving (Show)

setupDom :: MVar String -> JSM ()
setupDom commandVar = do
    Just doc <- currentDocument
    Just body <- getBody doc
    div <- uncheckedCastTo HTMLDivElement <$> createElement doc "div"
    heading <-
        uncheckedCastTo HTMLHeadingElement <$> do
            h1 <- createElement doc "h1"
            setAttribute h1 "id" "heading"
            setInnerHTML h1 "Hello and Welcome to AcidRain"
            return h1
    appendChild_ div heading
    info <-
        uncheckedCastTo HTMLParagraphElement <$> do
            p <- createElement doc "p"
            setInnerHTML
                p
                "Your health is deteriorating due to acid rain,\
             \ type \"potion\" or \"quit\""
            return p
    commandInput <-
        uncheckedCastTo HTMLInputElement <$> do
            input <- createElement doc "input"
            setAttribute input "id" "commandInput"
            setAttribute input "size" "8"
            return input
    _ <-
        on commandInput keyUp $ do
            ke <- ask
            result <- getKey ke
            when (result == "Enter") $ do
                value <- getValue commandInput
                liftIO $ putMVar commandVar value
    healthStatus <-
        uncheckedCastTo HTMLDivElement <$> do
            primeDiv <- createElement doc "div"
            setAttribute primeDiv "id" "healthStatus"
            return primeDiv
    commandErrorDiv <-
        uncheckedCastTo HTMLDivElement <$> do
            primeDiv <- createElement doc "div"
            setAttribute primeDiv "id" "commandErrorDiv"
            return primeDiv

    appendChild_ div info
    appendChild_ div commandInput
    appendChild_ div healthStatus
    appendChild_ div commandErrorDiv
    appendChild_ body div

userAction :: MonadAsync m => MVar String -> SerialT m GEvent
userAction commandVar = S.repeatM askUser
  where
    askUser = do
        window <- currentWindowUnchecked
        doc <- currentDocumentUnchecked
        command <- liftIO $ takeMVar commandVar
        case command of
            "potion" -> return (Heal 10)
            "harm" -> return (Harm 10)
            "quit" -> return Quit
            _ -> do
                commandErrorDiv <-
                    uncheckedCastTo HTMLDivElement <$>
                    getElementByIdUnchecked doc "commandErrorDiv"
                setInnerHTML commandErrorDiv "Type potion or harm or quit"
                askUser

acidRain :: MonadAsync m => SerialT m GEvent
acidRain = asyncly $ constRate 1 $ S.repeatM $ return $ Harm 1

data Result = Check | Done

runEvents :: (MonadState Int m, MonadAsync m) => MVar String -> SerialT m Result
runEvents commandVar = do
    event <- (userAction commandVar) `parallel` acidRain
    case event of
        Harm n -> modify (\h -> h - n) >> return Check
        Heal n -> modify (\h -> h + n) >> return Check
        Quit -> return Done

data Status = Alive | GameOver deriving Eq

getStatus :: (MonadState Int m, MonadAsync m) => Result -> m Status
getStatus result = do
    doc <- currentDocumentUnchecked
    healthStatus <-
        uncheckedCastTo HTMLDivElement <$>
        getElementByIdUnchecked doc "healthStatus"
    case result of
        Done -> setInnerHTML healthStatus "You quit!" >> return GameOver
        Check -> do
            health <- get
            if (health <= 0)
                then setInnerHTML healthStatus "You die!" >> return GameOver
                else setInnerHTML healthStatus ("Health = " <> show health) >>
                     return Alive

letItRain :: JSM ()
letItRain = do
    commandVar <- newEmptyMVar
    setupDom commandVar
    syncPoint
    let runGame =
            S.runWhile (== Alive) $ S.mapM getStatus (runEvents commandVar)
    void $ runStateT runGame 60

main :: JSM ()
main = letItRain
