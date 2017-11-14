module Test.Main where

import Prelude
import Halogen as H
import Svg.Attributes as SA
import Svg.Elements as SE
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)

-- smoke test

render :: forall t1 t2 t3 . t1 -> H.HTML t2 t3
render state =
  SE.svg [ SA.viewBox 0.0 0.0 100.0 100.0 ] [ SE.circle [ SA.r 10.0 ] ]

-- not a real test, just making sure things compile

main :: Eff (console :: CONSOLE) Unit
main = log "Nothing to see here"
