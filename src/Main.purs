module Main where

import Prelude
import Control.Monad.State (StateT,runStateT)
import Control.Monad.Trans.Class (lift)
import Data.Maybe (Maybe(..))
import Data.List as List
import Data.List ((:)) 
import Effect (Effect)
import Effect.Console (logShow)

import Control.Monad.ST (ST,run)
import Control.Monad.ST.Ref (STRef,new,read,write,modify)

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

foreign import mouse_event :: Event.Event -> Effect Unit

newtype Line = Line {
    y :: Number
} 


type Lines = List.List Line

type LinesRef = Ref.Ref Lines

-- { lines :: Lines, selected :: Maybe Line }

initLines :: Lines
initLines = List.Nil

oneLine :: Line
oneLine = Line { y: 10.0 } 

refx = Ref.new initLines

instance showLine :: Show Line where
  show (Line v) = "Line " <> show v 

type LineState = StateT (Int -> Int) Effect Unit

stdemo :: forall s. ST s (STRef s Int)
stdemo = 
    new 10
    --STRef.new (Line { y : 1.0 } : Line { y: 2.0 } : List.Nil) 

runx :: forall s. STRef s Int -> ST s Int
runx x = 
    --read x 
    modify (\t -> t * 2) x

    
runStdemo2 :: Int
runStdemo2 = run 
    (stdemo >>= \t ->
        runx t)

runStdemo :: Int
runStdemo = run do
    x <- stdemo
    runx x

{-
runStdemo2 :: forall s. ST s (STRef s Int) -> Int
runStdemo2 sx = run $
    stdemo >>= \t ->
        read t
        

test = 
    runStdemo2 stdemo
-}

main :: Effect Unit
main =
    getDoc >>= \doc ->
        getElementById "canvas" doc >>= \elTarget ->
            case elTarget of 
                Nothing -> 
                    logShow "OOPS"
                Just elx ->
                    --pure unit
                    refx >>= \refx_ -> 
                        EventTarget.eventListener (mouseEvent refx_) >>= \me -> 
                            EventTarget.addEventListener (EventType "mouseup") me false (toEventTarget elx) *>
                        EventTarget.eventListener (mouseEventDrag refx_) >>= \me -> 
                            EventTarget.addEventListener (EventType "mousemove") me false (toEventTarget elx) 
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

mx :: Event.Event -> LineState 
mx ev = pure unit

mx2 :: Int -> LineState 
mx2 i = pure unit

mouseEventDrag :: LinesRef -> Event.Event -> Effect Unit
mouseEventDrag lines event = 
    Ref.read lines >>= \lxx -> 
    logShow lxx

mouseEvent :: LinesRef -> Event.Event -> Effect Unit
mouseEvent lines event = 
    logShow "That's what I'm talking about!!!!!!!!!" *>
    Event.stopPropagation event *>
    Event.preventDefault event *>
    mouse_event(event) *>
    --runStateT mouseEvent2 List.Nil *>
    Ref.modify_ (\lx -> oneLine : lx) lines *>
    Ref.read lines >>= \lxx -> 
    logShow lxx *>
    pure unit
    
mouseEvent2 :: Event.Event -> LineState -- StateT Int Effect Unit
mouseEvent2 ev = 
    lift $ logShow "That's what I'm talking about AGAIN!!!!!!!!!" 

getDoc :: Effect NonElementParentNode
getDoc = 
    HTML.window >>= \win ->
        Window.document win >>= \doc ->
            pure $ HTMLDocument.toNonElementParentNode doc
