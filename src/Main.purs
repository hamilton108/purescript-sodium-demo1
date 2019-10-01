module Main where

import Prelude
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Console (logShow)
import Web.Event.Event (Event,EventType(..))
import Web.Event.EventTarget as EventTarget
-- import Graphics.Canvas as Canvas -- (Context2D,Canvas)
import Web.DOM.NonElementParentNode (NonElementParentNode,getElementById)
import Web.DOM.Element (toEventTarget)
import Web.HTML as HTML
import Web.HTML.Window as Window
import Web.HTML.HTMLDocument as HTMLDocument

foreign import mouse_event :: Event -> Effect Unit

ev = EventType "click"

main :: Effect Unit
main =
    getDoc >>= \doc ->
        getElementById "canvas" doc >>= \elTarget ->
            case elTarget of 
                Nothing -> logShow "OOPS"
                Just elx ->
                    EventTarget.eventListener mouseEvent >>= \me -> 
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

l = EventTarget.eventListener

mouseEvent :: Event -> Effect Unit
mouseEvent event = 
    logShow "That's what I'm talking about!!!!!!!!!" *>
    Event.stopPropagation event *>
    mouse_event(event) *>
    pure unit

getDoc :: Effect NonElementParentNode
getDoc = 
    HTML.window >>= \win ->
        Window.document win >>= \doc ->
            pure $ HTMLDocument.toNonElementParentNode doc
