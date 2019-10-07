module Main where

import Prelude
import Control.Monad.State (StateT,runStateT)
import Control.Monad.Trans.Class (lift)
import Data.Maybe (Maybe(..))
import Data.List as List
import Data.List ((:)) 
import Effect (Effect)
import Effect.Console (logShow)

import Control.Monad.ST as ST
import Control.Monad.ST.Ref as STRef

import Web.Event.Event (EventType(..))
import Web.Event.Event as Event
import Web.Event.EventTarget as EventTarget
-- import Graphics.Canvas as Canvas -- (Context2D,Canvas)
import Web.DOM.NonElementParentNode (NonElementParentNode,getElementById)
import Web.DOM.Element (toEventTarget)
import Web.HTML as HTML
import Web.HTML.Window as Window
import Web.HTML.HTMLDocument as HTMLDocument

foreign import mouse_event :: Event.Event -> Effect Unit

newtype Line = Line {
    y :: Number
} 

type Lines = List.List Line

instance showLine :: Show Line where
  show (Line v) = "Line " <> show v 

type LineState = StateT Lines Effect Unit

stdemo = 
    STRef.new (Line { y : 1.0 } : Line { y: 2.0 } : List.Nil) 

runStdemo = ST.run do
  x <- stdemo
  STRef.read x


main :: Effect Unit
main =
    getDoc >>= \doc ->
        getElementById "canvas" doc >>= \elTarget ->
            case elTarget of 
                Nothing -> logShow "OOPS"
                Just elx ->
                    EventTarget.eventListener mouseEvent >>= \me -> 
                        EventTarget.addEventListener (EventType "mouseup") me false (toEventTarget elx) 
                        -- EventTarget.addEventListener (EventType "mousemove") me false (toEventTarget elx) 
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

mouseEvent :: Event.Event -> Effect Unit
mouseEvent event = 
    logShow "That's what I'm talking about!!!!!!!!!" *>
    Event.stopPropagation event *>
    Event.preventDefault event *>
    mouse_event(event) *>
    runStateT mouseEvent2 List.Nil *>
    pure unit
    
mouseEvent2 :: LineState -- StateT Int Effect Unit
mouseEvent2 = 
    lift $ logShow "That's what I'm talking about AGAIN!!!!!!!!!" 

getDoc :: Effect NonElementParentNode
getDoc = 
    HTML.window >>= \win ->
        Window.document win >>= \doc ->
            pure $ HTMLDocument.toNonElementParentNode doc
