module Halogen.XLINK.Core where

import Prelude
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple)
import Halogen.HTML.Core (HTML, Prop(Attribute), Namespace(Namespace), AttrName(AttrName))
import Halogen.VDom (ElemName, VDom(Elem, Keyed))
import Unsafe.Coerce (unsafeCoerce)

-- | Core

xlinkNS :: Namespace
xlinkNS = Namespace "http://www.w3.org/1999/xlink"

attr :: forall i. AttrName -> String -> Prop i
attr (AttrName name) = Attribute (Just xlinkNS) name

-- | "xmlns:xlink" attribute declares xlink namespace availability and should be used on some parent element

xmlns_xlink :: forall i . Prop i
xmlns_xlink = Attribute Nothing "xmlns:xlink" "http://www.w3.org/1999/xlink"
