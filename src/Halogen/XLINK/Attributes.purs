module Halogen.XLINK.Attributes where

import Prelude

import Halogen.XLINK.Core as Core
import Data.Maybe (Maybe(..))
import Data.String (joinWith, toUpper)
import Halogen.HTML.Core (Prop, AttrName(AttrName))
import Halogen.HTML.Properties (IProp)
import Unsafe.Coerce (unsafeCoerce)

attr :: forall r i. AttrName -> String -> IProp r i
attr = coe Core.attr
  where
    coe :: (AttrName -> String -> Prop i) -> AttrName -> String -> IProp r i
    coe = unsafeCoerce

-- | These props are from Xlink specification (https://www.w3.org/TR/xlink11/) and should be used after `xmlns_xlink` is called

href :: forall i r. String -> IProp ( "xlink:href" :: String | r ) i
href = attr (AttrName "href")

type_ :: forall i r. String -> IProp ( "xlink:type" :: String | r ) i
type_ = attr (AttrName "type")

role :: forall i r. String -> IProp ( "xlink:role" :: String | r ) i
role = attr (AttrName "role")

title :: forall i r. String -> IProp ( "xlink:title" :: String | r ) i
title = attr (AttrName "title")

show :: forall i r. String -> IProp ( "xlink:show" :: String | r ) i
show = attr (AttrName "show")

actuate :: forall i r. String -> IProp ( "xlink:actuate" :: String | r ) i
actuate = attr (AttrName "actuate")
