module Svg.Attributes where

import Prelude
import Data.Maybe (Maybe(..))
import Data.String (joinWith)

import Core as Core

import Halogen.HTML.Core (Prop, AttrName(AttrName))
import Halogen.HTML.Properties (IProp, I)
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

cx :: forall r i. Number -> IProp (cx :: I | r) i
cx = attr (AttrName "cx") <<< show

cy :: forall r i. Number -> IProp (cy :: I | r) i
cy = attr (AttrName "cy") <<< show

r :: forall s i. Number -> IProp (r :: I | s) i
r = attr (AttrName "r") <<< show

viewBox :: forall r i. Number -> Number -> Number -> Number -> IProp (viewBox :: I | r) i
viewBox x y w h = attr (AttrName "viewBox") (joinWith " " $ map show [x, y, w, h])

rx :: forall r i. Number -> IProp (rx :: I | r) i
rx = attr (AttrName "rx") <<< show

ry :: forall r i. Number -> IProp (ry :: I | r) i
ry = attr (AttrName "ry") <<< show

width :: forall r i. Number -> IProp (width :: I | r) i
width = attr (AttrName "width") <<< show

height :: forall r i. Number -> IProp (height :: I | r) i
height = attr (AttrName "height") <<< show

x :: forall r i. Number -> IProp (x :: I | r) i
x = attr (AttrName "x") <<< show

y :: forall r i. Number -> IProp (y :: I | r) i
y = attr (AttrName "y") <<< show

stroke :: forall r i. Maybe Color -> IProp (stroke :: I | r) i
stroke = attr (AttrName "stroke") <<< printColor

fill :: forall r i. Maybe Color -> IProp (fill :: I | r) i
fill = attr (AttrName "fill") <<< printColor
