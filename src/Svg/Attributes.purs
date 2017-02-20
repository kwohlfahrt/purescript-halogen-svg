module Svg.Attributes where
-- Like Halogen.HTML.Properties

import Prelude
import Data.Maybe (Maybe(..))
import Data.String (joinWith)

import Core as Core

import Halogen.HTML.Core (Prop, AttrName(AttrName))
import Halogen.HTML.Properties (IProp)
import Unsafe.Coerce (unsafeCoerce)

data Color = RGB Int Int Int

printColor :: Maybe Color -> String
printColor (Just (RGB r g b)) = "rgb(" <> (joinWith "," $ map show [r, g, b]) <> ")"
printColor Nothing = "None"

attr :: forall r i. AttrName -> String -> IProp r i
attr = coe Core.attr
  where
    coe :: (AttrName -> String -> Prop i) -> AttrName -> String -> IProp r i
    coe = unsafeCoerce

cx :: forall r i. Number -> IProp (cx :: Number | r) i
cx = attr (AttrName "cx") <<< show

cy :: forall r i. Number -> IProp (cy :: Number | r) i
cy = attr (AttrName "cy") <<< show

r :: forall s i. Number -> IProp (r :: Number | s) i
r = attr (AttrName "r") <<< show

viewBox :: forall r i. Number -> Number -> Number -> Number -> IProp (viewBox :: String | r) i
viewBox x y w h = attr (AttrName "viewBox") (joinWith " " $ map show [x, y, w, h])

rx :: forall r i. Number -> IProp (rx :: Number | r) i
rx = attr (AttrName "rx") <<< show

ry :: forall r i. Number -> IProp (ry :: Number | r) i
ry = attr (AttrName "ry") <<< show

width :: forall r i. Number -> IProp (width :: Number | r) i
width = attr (AttrName "width") <<< show

height :: forall r i. Number -> IProp (height :: Number | r) i
height = attr (AttrName "height") <<< show

x :: forall r i. Number -> IProp (x :: Number | r) i
x = attr (AttrName "x") <<< show

y :: forall r i. Number -> IProp (y :: Number | r) i
y = attr (AttrName "y") <<< show

stroke :: forall r i. Maybe Color -> IProp (stroke :: String | r) i
stroke = attr (AttrName "stroke") <<< printColor

fill :: forall r i. Maybe Color -> IProp (fill :: String | r) i
fill = attr (AttrName "fill") <<< printColor
