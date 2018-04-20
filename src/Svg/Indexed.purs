module Svg.Indexed where
-- Like DOM.HTML.Indexed

import DOM.HTML.Indexed (MouseEvents)

type CoreAttributes r = (id :: String | r)
type GraphicalEventAttributes r = MouseEvents r

type SVGsvg = GraphicalEventAttributes (CoreAttributes (width :: Number, height :: Number, viewBox :: String))

type PresentationAttributes r = (stroke :: String, fill ::String | r)
type GlobalAttributes r = (PresentationAttributes (GraphicalEventAttributes (CoreAttributes (class ::String | r))))

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

type SVGpath = GlobalAttributes
  ( d :: String
  , transform :: String
  )

type SVGline = GlobalAttributes
  ( x1 :: Number
  , y1 :: Number
  , x2 :: Number
  , y2 :: Number
  , transform :: String
  )

type SVGtext = GlobalAttributes
  ( x :: Number
  , y :: Number
  , text_anchor :: String
  , dominant_baseline :: String
  , transform :: String
  )

type SVGforeignObject = GlobalAttributes
  ( x :: Number
  , y :: Number
  , height :: Number
  , width :: Number
  )
