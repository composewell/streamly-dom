{-# LANGUAGE FlexibleContexts #-}

module AcidRain.JSIO
    ( initConsole
    , printHealth
    , printError
    , userStream
    )
where

import Streamly
import Control.Monad.IO.Class (MonadIO(liftIO))

import Control.Monad (when)
import Control.Monad.Reader (ask)

import GHCJS.DOM
import GHCJS.DOM.Types
import GHCJS.DOM.KeyboardEvent
import GHCJS.DOM.Document
import GHCJS.DOM.Element
import GHCJS.DOM.HTMLInputElement
import GHCJS.DOM.NonElementParentNode
import GHCJS.DOM.Node
import GHCJS.DOM.EventM
import GHCJS.DOM.GlobalEventHandlers

import qualified Streamly.Internal.Prelude as S

-------------------------------------------------------------------------------
-- Heading (h1)
-------------------------------------------------------------------------------

makeHeading :: IO HTMLHeadingElement
makeHeading = do
    doc <- currentDocumentUnchecked
    uncheckedCastTo HTMLHeadingElement <$> do
        h1 <- createElement doc "h1"
        setAttribute h1 "id" "heading"
        setInnerHTML h1 "Hello and Welcome to AcidRain"
        return h1

-------------------------------------------------------------------------------
-- Info message (p)
-------------------------------------------------------------------------------

makeInfo :: IO HTMLParagraphElement
makeInfo = do
    doc <- currentDocumentUnchecked
    uncheckedCastTo HTMLParagraphElement <$> do
        p <- createElement doc "p"
        setInnerHTML
            p $ "Your health is deteriorating due to acid rain,"
                 ++ " type \"potion\" or \"quit\""
        return p

-------------------------------------------------------------------------------
-- Health meter (div)
-------------------------------------------------------------------------------

-- XXX instead of searching every time we can pass the dom elements directly
-- via state monad.
healthStatusDiv :: String
healthStatusDiv = "healthStatus"

makeHealthStatus :: IO HTMLDivElement
makeHealthStatus = do
    doc <- currentDocumentUnchecked
    uncheckedCastTo HTMLDivElement <$> do
        primeDiv <- createElement doc "div"
        setAttribute primeDiv "id" healthStatusDiv
        return primeDiv

printHealth :: (MonadIO m, ToJSString a) => a -> m ()
printHealth msg = do
    doc <- currentDocumentUnchecked
    healthStatus <-
        uncheckedCastTo HTMLDivElement <$>
        getElementByIdUnchecked doc healthStatusDiv
    setInnerHTML healthStatus msg

-------------------------------------------------------------------------------
-- Error display (div)
-------------------------------------------------------------------------------

errorDisplayDiv :: String
errorDisplayDiv = "errorDisplayDiv"

makeErrorDisplay :: IO HTMLDivElement
makeErrorDisplay = do
    doc <- currentDocumentUnchecked
    uncheckedCastTo HTMLDivElement <$> do
        primeDiv <- createElement doc "div"
        setAttribute primeDiv "id" errorDisplayDiv
        return primeDiv

printError :: (MonadIO m, ToJSString a) => a -> m ()
printError msg = do
    doc <- currentDocumentUnchecked
    el <-
        uncheckedCastTo HTMLDivElement <$>
        getElementByIdUnchecked doc errorDisplayDiv
    setInnerHTML el msg

-------------------------------------------------------------------------------
-- User input (input)
-------------------------------------------------------------------------------

commandInput :: String
commandInput = "commandInput"

makeInput :: IO HTMLInputElement
makeInput = do
    doc <- currentDocumentUnchecked
    uncheckedCastTo HTMLInputElement <$> do
        inp <- createElement doc "input"
        setAttribute inp "id" commandInput
        setAttribute inp "size" "8"
        return inp

-- | Call the user supplied callback on command input.
setEventHandler :: (String -> IO ()) -> IO ()
setEventHandler callback = do
    doc <- currentDocumentUnchecked
    el <-
        uncheckedCastTo HTMLInputElement <$>
            getElementByIdUnchecked doc commandInput
    _ <- on el keyUp $ do
            k <- ask
            result <- getKey k
            when (result == "Enter") $ liftIO $ do
                value <- getValue el
                callback value
    return ()

userStream :: MonadAsync m => SerialT m String
userStream = S.hoist liftIO $ S.fromCallback (setEventHandler)

-------------------------------------------------------------------------------
-- Combined elements (div)
-------------------------------------------------------------------------------

-- XXX make childs using monadic actions and append them
-- appendChildsM_ :: self -> [m node] -> m ()

setupPage :: IO HTMLDivElement
setupPage = do
    doc <- currentDocumentUnchecked
    el <- uncheckedCastTo HTMLDivElement <$> createElement doc "div"

    makeHeading >>= appendChild_ el
    makeInfo >>= appendChild_ el
    makeHealthStatus >>= appendChild_ el
    makeErrorDisplay >>= appendChild_ el
    makeInput >>= appendChild_ el
    return el

-------------------------------------------------------------------------------
-- Document body
-------------------------------------------------------------------------------

appendToBody :: IsNode node => node -> JSM ()
appendToBody child = do
    Just doc <- currentDocument
    Just body <- getBody doc
    appendChild_ body child

initConsole :: IO ()
initConsole = do
    setupPage >>= appendToBody
    syncPoint
