module Main where

import Prelude
import Data.Maybe (Maybe(..))
import Data.List as List
import Data.List ((:)) 
import Effect (Effect)
import Effect.Console (logShow)

import Web.Event.Event (EventType(..))
import Web.Event.Event as Event
import Web.Event.EventTarget as EventTarget
import Effect.Ref as Ref
-- import Graphics.Canvas as Canvas -- (Context2D,Canvas)
import Web.DOM.NonElementParentNode (NonElementParentNode,getElementById)
import Web.DOM.Element (toEventTarget)
import Web.HTML as HTML
import Web.HTML.Window as Window
import Web.HTML.HTMLDocument as HTMLDocument

{-
import Data.IORef (newIORef,modifyIORef,readIORef)

type Counter = Int -> IO Int

makeCounter :: IO Counter
makeCounter = do
    r <- newIORef 0
    return (\i -> do modifyIORef r (\q -> q + i) -- (+i)
                    readIORef r)

testCounter :: Counter -> IO ()
testCounter counter = do
  b <- counter 1
  c <- counter 1
  d <- counter 1
  print [b,c,d]

main = do
  counter <- makeCounter
  testCounter counter
  testCounter counter
-}


newtype Line = 
    Line 
    { y :: Number
    , draggable :: Boolean
    } 

foreign import mouse_event :: Event.Event -> Boolean -> Effect Line

instance showLine :: Show Line where
    show (Line v) = "Line: " <> show v 

newtype Lines = 
    Lines
    { lines :: List.List Line
    , selected :: Maybe Line 
    }

instance showLines :: Show Lines where
    show (Lines { lines, selected }) = "Lines, " <> show lines <> ", selected: " <> show selected

type LinesRef = Ref.Ref Lines

initLines :: Lines
initLines = 
    Lines
    { lines : List.Nil
    , selected : Nothing
    }

linesRef :: Effect (Ref.Ref Lines)
linesRef = Ref.new initLines

type EventListeners = List.List EventTarget.EventListener

type EventListenerRef = Ref.Ref EventListeners

eventListenerRef :: Effect EventListenerRef
eventListenerRef = 
    Ref.new List.Nil

addEventListenerRef :: EventListenerRef -> EventTarget.EventListener -> Effect Unit
addEventListenerRef lref listener = 
    Ref.modify_ (\listeners -> listener : listeners) lref

initMouseEvents :: NonElementParentNode -> Effect Unit
initMouseEvents doc = 
    getElementById "canvas" doc >>= \elTarget ->
        case elTarget of 
            Nothing -> 
                logShow "OOPS"
            Just elx ->
                eventListenerRef >>= \elr ->
                    linesRef >>= \ lir -> 
                        EventTarget.eventListener (mouseEventAddLine lir) >>= \me1 -> 
                            addEventListenerRef elr me1 *> 
                            EventTarget.addEventListener (EventType "mouseup") me1 false (toEventTarget elx) 
                        --EventTarget.eventListener (mouseEventDrag lir) >>= \me2 -> 
                        --    EventTarget.addEventListener (EventType "mousemove") me2 false (toEventTarget elx) 

initButtonEvent :: NonElementParentNode -> Effect Unit
initButtonEvent doc = 
    getElementById "button" doc >>= \buttonTarget ->
        case buttonTarget of 
            Nothing -> 
                logShow "OOPS AGAIN"
            Just button ->
                EventTarget.eventListener buttonEvent >>= \b1 -> 
                    EventTarget.addEventListener (EventType "click") b1 false (toEventTarget button) 

initEvents :: Effect Unit
initEvents =
    getDoc >>= \doc ->
        initMouseEvents doc *> 
        initButtonEvent doc 
{-
        getElementById "button" doc >>= \buttonTarget ->
            case buttonTarget of 
                Nothing -> 
                    logShow "OOPS AGAIN"
                Just button ->
                    EventTarget.eventListener buttonEvent >>= \b1 -> 
                        EventTarget.addEventListener (EventType "click") b1 false (toEventTarget button) 
-}

main :: Effect Unit
main =
    initEvents

 {-

    let 
        curId = "canvas"    
    in
    NonElementParentNode.getElementById curId >>= \canvas ->
    case canvas of
        Nothing -> 
            logShow $ "CanvasId " <> curId <> " does not exist!"
        Just canvax ->
            logShow ("Drawing canvas: " <> curId) 

-}

buttonEvent :: Event.Event -> Effect Unit
buttonEvent event = 
    logShow "BUTTON EVENT!!!"

defaultEventHandling :: Event.Event -> Effect Unit
defaultEventHandling event = 
    Event.stopPropagation event *>
    Event.preventDefault event 

mouseEventDrag :: LinesRef -> Event.Event -> Effect Unit
mouseEventDrag lref event = 
    defaultEventHandling event *>
    Ref.read lref >>= \lxx -> 
    logShow lxx

addLine_ :: Line -> Lines -> Lines
addLine_ newLine (Lines l@{lines,selected}) = 
    Lines $ l { lines = newLine : lines } 

addLine :: LinesRef -> Event.Event -> Effect Unit
addLine lref event =
    mouse_event event true >>= \newLine -> 
    Ref.modify_ (addLine_  newLine) lref *>
    Ref.read lref >>= \lxx -> 
    logShow lxx 

mouseEventAddLine :: LinesRef -> Event.Event -> Effect Unit
mouseEventAddLine lref event = 
    logShow "That's what I'm talking about!!!!!!!!!" *>
    defaultEventHandling event *>
    addLine lref event 
    
getDoc :: Effect NonElementParentNode
getDoc = 
    HTML.window >>= \win ->
        Window.document win >>= \doc ->
            pure $ HTMLDocument.toNonElementParentNode doc
