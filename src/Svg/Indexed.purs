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
  , transform :: String
  )

type SVGrect = Interactive
  ( x :: Number
  , y :: Number
  , width :: Number
  , height :: Number
  , stroke :: String
  , fill :: String
  , transform :: String
  )

type SVGg = Interactive
  ( transform :: String )

type SVGpath = Interactive
  ( d :: String
  , stroke :: String
  , fill :: String
  , transform :: String
  )

type SVGtext = Interactive
  ( x :: Number
  , y :: Number
  , text_anchor :: String
  , dominant_baseline :: String
  , stroke :: String
  , fill :: String
  , transform :: String
  )

type SVGforeignObject = Interactive
  ( x :: Number
  , y :: Number
  , height :: Number
  , width :: Number
  )
