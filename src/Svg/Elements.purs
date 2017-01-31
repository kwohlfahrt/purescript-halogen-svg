module Svg.Elements where

import Prelude

import Core as Core

import Halogen.HTML.Core (HTML, Prop, ElemName(ElemName))
import Halogen.HTML.Elements (Node, Leaf)
import Halogen.HTML.Properties (IProp, I)
import Unsafe.Coerce (unsafeCoerce)

element :: forall r p i. ElemName -> Array (IProp r i) -> Array (HTML p i) -> HTML p i
element = coe Core.element
  where
    coe :: (ElemName -> Array (Prop i) -> Array (HTML p i) -> HTML p i)
        -> ElemName -> Array (IProp r i) -> Array (HTML p i) -> HTML p i
    coe = unsafeCoerce

svg :: forall p i. Node (viewBox :: I) p i
svg = element $ ElemName "svg"

circle :: forall p i. Leaf (cx :: I, cy :: I, r :: I, stroke :: I, fill :: I) p i
circle props = element (ElemName "circle") props []

rect :: forall p i. Leaf (x :: I, y :: I, width :: I, height :: I, stroke :: I, fill :: I) p i
rect props = element (ElemName "rect") props []
