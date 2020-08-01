module Halogen.SVG.Core where

import Prelude
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple)
import Halogen.HTML.Core (HTML, Prop(Attribute), Namespace(Namespace), AttrName(AttrName))
import Halogen.VDom (ElemName, VDom(Elem, Keyed))
import Unsafe.Coerce (unsafeCoerce)

svgNS :: Namespace
svgNS = Namespace "http://www.w3.org/2000/svg"

element :: forall w i. ElemName -> Array (Prop i) -> Array (HTML w i) -> HTML w i
element =
  coe (\name props children -> Elem (Just svgNS) name props children)
  where
  coe
    :: (ElemName -> Array (Prop i) -> Array (VDom (Array (Prop i)) w) -> VDom (Array (Prop i)) w)
    -> ElemName -> Array (Prop i) -> Array (HTML w i) -> HTML w i
  coe = unsafeCoerce

attr :: forall i. AttrName -> String -> Prop i
attr (AttrName name) = Attribute Nothing name

keyedElement :: forall p i. ElemName -> Array (Prop i) -> Array (Tuple String (HTML p i)) -> HTML p i
keyedElement = coe (\name props children -> Keyed (Just svgNS) name props children)
  where
    coe :: (ElemName -> Array (Prop i) -> Array (Tuple String (VDom (Array (Prop i)) p)) -> VDom (Array (Prop i)) p)
        -> ElemName -> Array (Prop i) -> Array (Tuple String (HTML p i)) -> HTML p i
    coe = unsafeCoerce
