module Svg.Indexed where
-- Like DOM.HTML.Indexed

import DOM.HTML.Indexed (Interactive)

type SVGsvg = Interactive (viewBox :: String)

type SVGcircle = Interactive
  ( cx :: Number
  , cy :: Number
  , r :: Number
  , stroke :: String
  , fill :: String
  )

type SVGrect = Interactive
  ( x :: Number
  , y :: Number
  , width :: Number
  , height :: Number
  , stroke :: String
  , fill :: String
  )

type SVGg = Interactive ()
