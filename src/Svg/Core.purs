module Core where
-- Like Halogen.HTML.Core

import Prelude
import Data.Maybe (Maybe(..))
import Halogen.HTML.Core (HTML, Prop(Attribute), Namespace(Namespace), AttrName(AttrName))
import Halogen.VDom (ElemName, VDom(Elem))
import Unsafe.Coerce (unsafeCoerce)

ns :: Maybe Namespace
ns = Just $ Namespace "http://www.w3.org/2000/svg"

element :: forall w i. ElemName -> Array (Prop i) -> Array (HTML w i) -> HTML w i
element =
  coe (\name props children -> Elem ns name props children)
  where
  coe
    :: (ElemName -> Array (Prop i) -> Array (VDom (Array (Prop i)) w) -> VDom (Array (Prop i)) w)
    -> ElemName -> Array (Prop i) -> Array (HTML w i) -> HTML w i
  coe = unsafeCoerce

         

attr :: forall i. AttrName -> String -> Prop i
attr (AttrName name) = Attribute Nothing name
