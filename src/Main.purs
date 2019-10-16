module Main where

import Prelude
import Data.Maybe (Maybe(..))
import Data.List as List
import Data.List ((:)) 
import Effect (Effect)
import Effect.Console (logShow)
import Data.Traversable as Traversable

import Web.Event.Event (EventType(..))
import Web.Event.Event as Event
import Web.Event.EventTarget as EventTarget
import Effect.Ref as Ref
-- import Graphics.Canvas as Canvas -- (Context2D,Canvas)
import Web.DOM.NonElementParentNode (NonElementParentNode,getElementById)
import Web.DOM.Element (toEventTarget,Element)
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

foreign import createLine :: Event.Event -> Effect Line

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

{-
newtype EventListenerInfo =
    EventListenerInfo 
    { listener :: EventTarget.EventListener
    , eventType :: EventType
    }
-}

type EventListenerInfo = EventTarget.EventListener

type EventListeners = List.List EventListenerInfo 

type EventListenerRef = Ref.Ref EventListeners

eventListenerRef :: Effect EventListenerRef
eventListenerRef = 
    Ref.new List.Nil

addEventListenerRef :: EventListenerRef -> EventTarget.EventListener -> Effect Unit
addEventListenerRef lref listener = 
    Ref.modify_ (\listeners -> listener : listeners) lref

initMouseEvents :: Element -> EventListenerRef -> Effect Unit
initMouseEvents target elr = 
    linesRef >>= \lir -> 
        EventTarget.eventListener (mouseEventAddLine lir) >>= \me1 -> 
            EventTarget.addEventListener (EventType "mouseup") me1 false (toEventTarget target) *>
            addEventListenerRef elr me1 

unlisten :: Element -> EventListenerInfo -> Effect Unit
unlisten target info = 
    EventTarget.removeEventListener (EventType "mouseup") info false (toEventTarget target)

unlistener :: Element -> EventListenerRef -> Int -> Effect Unit
unlistener target elr dummy =
    let 
        unlisten1 = unlisten target
    in
    Ref.read elr >>= \elrx -> 
        Traversable.traverse_ unlisten1 elrx


initEvents :: Effect (Int -> Effect Unit)
initEvents =
    getDoc >>= \doc ->
        getElementById "canvas" doc >>= \target ->
            case target of 
                Nothing -> 
                    pure (\t -> pure unit) 
                Just targetx ->
                    eventListenerRef >>= \elr ->
                        initMouseEvents targetx elr *>
                            pure (unlistener targetx elr)

main :: Effect Unit
main =
    logShow "Main" 


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
    createLine event >>= \newLine -> 
    Ref.modify_ (addLine_  newLine) lref 
    --Ref.read lref >>= \lxx -> 
    --logShow lxx 

mouseEventAddLine :: LinesRef -> Event.Event -> Effect Unit
mouseEventAddLine lref event = 
    defaultEventHandling event *>
    addLine lref event 
    
getDoc :: Effect NonElementParentNode
getDoc = 
    HTML.window >>= \win ->
        Window.document win >>= \doc ->
            pure $ HTMLDocument.toNonElementParentNode doc
