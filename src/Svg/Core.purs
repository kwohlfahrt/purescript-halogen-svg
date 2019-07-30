module Core where
-- Like Halogen.HTML.Core

import Prelude
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple)
import Halogen.HTML.Core (HTML, Prop(Attribute), Namespace(Namespace), AttrName(AttrName))
import Halogen.VDom (ElemName, ElemSpec(ElemSpec), VDom(Elem, Keyed))
import Unsafe.Coerce (unsafeCoerce)

ns :: Maybe Namespace
ns = Just $ Namespace "http://www.w3.org/2000/svg"

element :: forall p i. ElemName -> Array (Prop i) -> Array (HTML p i) -> HTML p i
element = coe (\name props children -> Elem (ElemSpec ns name props) children)
  where
    coe :: (ElemName -> Array (Prop i) -> Array (VDom (Array (Prop i)) p) -> VDom (Array (Prop i)) p)
        -> ElemName -> Array (Prop i) -> Array (HTML p i) -> HTML p i
    coe = unsafeCoerce

attr :: forall i. AttrName -> String -> Prop i
attr (AttrName name) = Attribute Nothing name

keyedElement :: forall p i. ElemName -> Array (Prop i) -> Array (Tuple String (HTML p i)) -> HTML p i
keyedElement = coe (\name props children -> Keyed (ElemSpec ns name props) children)
  where
    coe :: (ElemName -> Array (Prop i) -> Array (Tuple String (VDom (Array (Prop i)) p)) -> VDom (Array (Prop i)) p)
        -> ElemName -> Array (Prop i) -> Array (Tuple String (HTML p i)) -> HTML p i
    coe = unsafeCoerce
