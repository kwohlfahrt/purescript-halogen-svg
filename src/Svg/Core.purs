module Core where

import Prelude
import Data.Maybe (Maybe(..))

import Halogen.HTML.Core (HTML(Element), Prop(Attr), Namespace, namespace,
                          TagName, AttrName)

element :: forall p i. TagName -> Array (Prop i) -> Array (HTML p i) -> HTML p i
element = Element ns
  where
    ns :: Maybe Namespace
    ns = Just $ namespace "http://www.w3.org/2000/svg"

attr :: forall i. AttrName -> String -> Prop i
attr = Attr Nothing
