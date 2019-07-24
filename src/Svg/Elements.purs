module Svg.Elements where
-- Like Halogen.HTML.Elements

import Prelude

import Core as Core

import Halogen.HTML.Core (HTML, Prop, ElemName(ElemName))
import Halogen.HTML.Elements (Node, Leaf)
import Halogen.HTML.Properties (IProp)
import Unsafe.Coerce (unsafeCoerce)
import Svg.Indexed as I

element :: forall r p i. ElemName -> Array (IProp r i) -> Array (HTML p i) -> HTML p i
element = coe Core.element
  where
    coe :: (ElemName -> Array (Prop i) -> Array (HTML p i) -> HTML p i)
        -> ElemName -> Array (IProp r i) -> Array (HTML p i) -> HTML p i
    coe = unsafeCoerce

svg :: forall p i. Node I.SVGsvg p i
svg = element $ ElemName "svg"

g :: forall p i. Node I.SVGg p i
g = element $ ElemName "g"

circle :: forall p i. Leaf I.SVGcircle p i
circle props = element (ElemName "circle") props []

ellipse :: forall p i. Leaf I.SVGellipse p i
ellipse props = element (ElemName "ellipse") props []

rect :: forall p i. Leaf I.SVGrect p i
rect props = element (ElemName "rect") props []

path :: forall p i. Leaf I.SVGpath p i
path props = element (ElemName "path") props []

line :: forall p i. Leaf I.SVGline p i
line props = element (ElemName "line") props []

text :: forall p i. Node I.SVGtext p i
text = element (ElemName "text")

foreignObject :: forall p i . Node I.SVGforeignObject p i
foreignObject = element (ElemName "foreignObject")

marker :: forall p i. Node I.SVGmarker p i
marker = element (ElemName "marker")

defs :: forall p i. Array (HTML p i) -> HTML p i
defs = element (ElemName "defs") []
