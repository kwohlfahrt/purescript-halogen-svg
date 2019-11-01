module Test.Main where

import Prelude
import Halogen.HTML as HH
import Svg.Attributes as SA
import Svg.Elements as SE
import Effect (Effect)
import Effect.Console (log)

-- smoke test

render :: forall t1 t2 t3 . t1 -> HH.HTML t2 t3
render state =
  SE.svg [ SA.viewBox 0.0 0.0 100.0 100.0 ] [ SE.circle [ SA.r 10.0 ] ]

-- not a real test, just making sure things compile

main :: Effect Unit
main = log "Nothing to see here"
