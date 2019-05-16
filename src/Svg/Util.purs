module Svg.Util where

import Data.Vec3 (Vec3)
import Prelude ((<<<))
import Effect.Aff (Aff)
import Effect.Aff.Compat (EffectFnAff, fromEffectFnAff)
import Web.HTML (HTMLElement)

type CssSelector = String

foreign import _beginElements :: CssSelector -> EffectFnAff Int

beginElements :: CssSelector -> Aff Int
beginElements = fromEffectFnAff <<< _beginElements

foreign import domToSvgCoordinates :: HTMLElement -> Vec3 Int -> Vec3 Int
