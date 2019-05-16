module Svg.Indexed where

import Web.UIEvent.MouseEvent (MouseEvent)
import Web.UIEvent.WheelEvent (WheelEvent)
import Web.UIEvent.KeyboardEvent (KeyboardEvent)

-- Attributes based on Mozilla MDN categories

type CoreAttributes r = (id :: String | r)

type StyleAttributes r = ("class" :: String | r)

-- Subset of events that work on Firefox 60/Chromium 66
type GlobalEventAttributes r =
  ( onClick :: MouseEvent
  , onDoubleClick :: MouseEvent
  , onContextMenu :: MouseEvent
  , onKeyDown :: KeyboardEvent
  , onKeyPress :: KeyboardEvent
  , onKeyUp :: KeyboardEvent
  , onMouseDown :: MouseEvent
  , onMouseEnter :: MouseEvent
  , onMouseLeave :: MouseEvent
  , onMouseMove :: MouseEvent
  , onMouseOut :: MouseEvent
  , onMouseOver :: MouseEvent
  , onMouseUp :: MouseEvent
  , onWheel :: WheelEvent
  | r)

-- These can also be done with CSS
type PresentationAttributes r = (stroke :: String, fill :: String | r)

type MarkerAttributes r =
  ( markerStart :: String
  , markerMid :: String
  , markerEnd :: String
  | r
  )

type GlobalAttributes r = (PresentationAttributes (GlobalEventAttributes (StyleAttributes (CoreAttributes r))))

type SVGsvg = GlobalAttributes
  ( width :: Number
  , height :: Number
  , viewBox :: String
  , preserveAspectRatio :: String
  )

type SVGcircle = GlobalAttributes
  ( cx :: Number
  , cy :: Number
  , r :: Number
  , transform :: String
  )

type SVGellipse = GlobalAttributes
  ( cx :: Number
  , cy :: Number
  , rx :: Number
  , ry :: Number
  , transform :: String
  )

type SVGrect = GlobalAttributes
  ( x :: Number
  , y :: Number
  , rx :: Number
  , ry :: Number
  , width :: Number
  , height :: Number
  , transform :: String
  )

type SVGg = GlobalAttributes
  ( transform :: String )

type SVGpath = MarkerAttributes (GlobalAttributes
  ( d :: String
  , transform :: String
  ))

type SVGline = MarkerAttributes (GlobalAttributes
  ( x1 :: Number
  , y1 :: Number
  , x2 :: Number
  , y2 :: Number
  , transform :: String
  , strokeWidth :: Number
  ))

type SVGtext = GlobalAttributes
  ( x :: Number
  , y :: Number
  , text_anchor :: String
  , dominant_baseline :: String
  , transform :: String
  , font_size :: String
  )

type SVGforeignObject = GlobalAttributes
  ( x :: Number
  , y :: Number
  , height :: Number
  , width :: Number
  )

type SVGmarker = (PresentationAttributes (StyleAttributes (CoreAttributes
  ( markerWidth :: Number
  , markerHeight :: Number
  , strokeWidth :: Number
  , refX :: Number
  , refY :: Number
  , orient :: String
  , markerUnits :: String
  ))))

--------------------------------------------------------------------------------

type AnimationAttributes r = GlobalAttributes
  ( from :: String
  , to :: String
  , begin :: String
  , dur :: String
  , repeatCount :: Int
  , fill :: String -- ^ Unlike 'fill' in 'GlobalAttributes', this is intended to record a 'FillState' via 'fillAnim'.
  | r
  )

type SVGanimate = AnimationAttributes (attributeName :: String)

type SVGanimateMotion = AnimationAttributes (path :: String)

-- TODO should this have GlobalAttributes?
type SVGmpath = (xlinkHref :: String)

--------------------------------------------------------------------------------

type SVGtitle = GlobalAttributes ()
