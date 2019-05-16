module Svg.Attributes where
-- Like Halogen.HTML.Properties

import Prelude
import Data.Maybe (Maybe(..), maybe)
import Data.String (joinWith, toUpper)

import Core as Core

import Halogen.HTML.Core (Prop, AttrName(AttrName), Namespace(Namespace))
import Halogen.HTML.Properties (IProp, attrNS)
import Unsafe.Coerce (unsafeCoerce)

data Color = RGB Int Int Int
           | RGBA Int Int Int Number

printColor :: Maybe Color -> String
printColor (Just (RGB r g b)) = "rgb(" <> (joinWith "," $ map show [r, g, b]) <> ")"
printColor (Just (RGBA r g b o)) = "rgba(" <> (joinWith "," $ map show [r, g, b]) <> "," <> show o <> ")"
printColor Nothing = "None"

data Transform
  = Matrix Number Number Number Number Number Number
  | Translate Number Number
  | Scale Number Number
  | Rotate Number Number Number
  | SkewX Number
  | SkewY Number

data TextAnchor = Start | AnchorMiddle | End

data CSSLength
  = Cm Number
  | Mm Number
  | Inches Number
  | Px Number
  | Pt Number
  | Pc Number
  | Em Number
  | Ex Number
  | Rem Number
  | Vw Number
  | Vh Number
  | Vmin Number
  | Vmax Number
  | Pct Number
  | Nil

instance showCSSLength :: Show CSSLength where
  show (Cm i) = (show i) <> "cm"
  show (Mm i) = (show i) <> "mm"
  show (Inches i) = (show i) <> "in"
  show (Px i) = (show i) <> "px"
  show (Pt i) = (show i) <> "pt"
  show (Pc i) = (show i) <> "pc"
  show (Em i) = (show i) <> "em"
  show (Ex i) = (show i) <> "ex"
  show (Rem i) = (show i) <> "rem"
  show (Vw i) = (show i) <> "vw"
  show (Vh i) = (show i) <> "vh"
  show (Vmin i) = (show i) <> "vmin"
  show (Vmax i) = (show i) <> "vmax"
  show (Pct i) = (show i) <> "%"
  show Nil = "0"

data FontSize
  = XXSmall
  | XSmall
  | Small
  | Medium
  | Large
  | XLarge
  | XXLarge
  | Smaller
  | Larger
  | FontSizeLength CSSLength

data Orient
  = AutoOrient
  | AutoStartReverse

instance showOrient :: Show Orient where
  show AutoOrient = "auto"
  show AutoStartReverse = "auto-start-reverse"

printOrient :: Orient -> String
printOrient AutoOrient = "auto"
printOrient AutoStartReverse = "auto-start-reverse"

data MarkerUnit
  = UserSpaceOnUse
  | StrokeWidth

instance showMarkerUnit :: Show MarkerUnit where
  show UserSpaceOnUse = "userSpaceOnUse"
  show StrokeWidth = "strokeWidth"

printMarkerUnit :: MarkerUnit -> String
printMarkerUnit UserSpaceOnUse = "userSpaceOnUse"
printMarkerUnit StrokeWidth = "strokeWidth"

instance showFontSize :: Show FontSize where
  show XXSmall = "xx-small"
  show XSmall = "x-small"
  show Small = "small"
  show Medium = "medium"
  show Large = "large"
  show XLarge = "x-large"
  show XXLarge = "xx-large"
  show Smaller = "smaller"
  show Larger = "larger"
  show (FontSizeLength l) = show l

printTextAnchor :: TextAnchor -> String
printTextAnchor Start = "start"
printTextAnchor AnchorMiddle = "middle"
printTextAnchor End = "end"

data Baseline
  = Auto | UseScript | NoChange | ResetSize | Ideographic | Alphabetic | Hanging
  | Mathematical | Central | BaselineMiddle | TextAfterEdge | TextBeforeEdge

printBaseline :: Baseline -> String
printBaseline Auto = "auto"
printBaseline UseScript = "use-script"
printBaseline NoChange = "no-change"
printBaseline ResetSize = "reset-size"
printBaseline Ideographic = "ideographic"
printBaseline Alphabetic = "alphabetic"
printBaseline Hanging = "hanging"
printBaseline Mathematical = "mathematical"
printBaseline Central = "central"
printBaseline BaselineMiddle = "middle"
printBaseline TextAfterEdge = "text-after-edge"
printBaseline TextBeforeEdge = "text-before-edge"

printTransform :: Transform -> String
printTransform (Matrix a b c d e f) =
  "matrix(" <> (joinWith "," $ map show [a, b, c, d, e, f]) <> ")"
printTransform (Translate x y) = "translate(" <> (joinWith "," $ map show [x, y]) <> ")"
printTransform (Scale x y) = "scale(" <> (joinWith "," $ map show [x, y]) <> ")"
printTransform (Rotate a x y) = "rotate(" <> (joinWith "," $ map show [a, x, y]) <> ")"
printTransform (SkewX a) = "skewX(" <> show a <> ")"
printTransform (SkewY a) = "skewY(" <> show a <> ")"

data D = Rel Command | Abs Command
printD :: D -> String
printD (Abs cmd) = (toUpper p.command) <> p.params
  where p = printCommand cmd
printD (Rel cmd) = p.command <> p.params
  where p = printCommand cmd

data Command
  = M Number Number
  | L Number Number
  | C Number Number Number Number Number Number
  | S Number Number Number Number
  | Q Number Number Number Number
  | T Number Number
  | A Number Number Number Boolean Boolean Number Number
  | Z

printCommand :: Command -> {command :: String, params :: String}
printCommand (M x y) = {command: "m", params: joinWith "," $ map show [x, y]}
printCommand (L x y) = {command: "l", params: joinWith "," $ map show [x, y]}
printCommand (C x1 y1 x2 y2 x y) =
  {command: "c" , params: joinWith "," $ map show [x1, y1, x2, y2, x, y]}
printCommand (S x2 y2 x y) =
  {command: "s" , params: joinWith "," $ map show [x2, y2, x, y]}
printCommand (Q x1 y1 x y) =
  {command: "q" , params: joinWith "," $ map show [x1, y1, x, y]}
printCommand (T x y) = {command: "t", params: joinWith "," $ map show [x, y]}
printCommand (A rx ry rot large sweep x y) =
  {command: "a", params: joinWith ","
                 $ map show [ rx, ry, rot ]
                 <> [ large_flag, sweep_flag ]
                 <> map show [ x, y ]}
  where
  large_flag = if large then "0" else "1"
  sweep_flag = if sweep then "0" else "1"
printCommand Z = {command: "z", params: ""}

data Align = Min | Mid | Max

printAlign :: Align -> String
printAlign Min = "Min"
printAlign Mid = "Mid"
printAlign Max = "Max"

data MeetOrSlice = Meet | Slice
printMeetOrSlice :: MeetOrSlice -> String
printMeetOrSlice Meet = "meet"
printMeetOrSlice Slice = "slice"

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

preserveAspectRatio :: forall r i. Maybe {x :: Align, y :: Align} -> MeetOrSlice -> IProp (preserveAspectRatio :: String | r) i
preserveAspectRatio align slice =
  attr (AttrName "preserveAspectRatio") (joinWith " " $ [align_str, printMeetOrSlice slice])
  where
    align_str = case align of
      Nothing -> "none"
      Just {x, y} -> joinWith "" $ ["x", printAlign x, "Y", printAlign y]

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

x1 :: forall r i. Number -> IProp (x1 :: Number | r) i
x1 = attr (AttrName "x1") <<< show

y1 :: forall r i. Number -> IProp (y1 :: Number | r) i
y1 = attr (AttrName "y1") <<< show

x2 :: forall r i. Number -> IProp (x2 :: Number | r) i
x2 = attr (AttrName "x2") <<< show

y2 :: forall r i. Number -> IProp (y2 :: Number | r) i
y2 = attr (AttrName "y2") <<< show

stroke :: forall r i. Maybe Color -> IProp (stroke :: String | r) i
stroke = attr (AttrName "stroke") <<< printColor

fill :: forall r i. Maybe Color -> IProp (fill :: String | r) i
fill = attr (AttrName "fill") <<< printColor

transform :: forall r i . Array Transform -> IProp (transform :: String | r) i
transform = attr (AttrName "transform") <<< joinWith " " <<< map printTransform

d :: forall r i . Array D -> IProp (d :: String | r) i
d = attr (AttrName "d") <<< joinWith " " <<< map printD

text_anchor :: forall r i . TextAnchor -> IProp (text_anchor :: String | r) i
text_anchor = attr (AttrName "text-anchor") <<< printTextAnchor

font_size :: forall r i. FontSize -> IProp (font_size :: String | r) i
font_size = attr (AttrName "font-size") <<< show

dominant_baseline :: forall r i . Baseline -> IProp (transform :: String | r) i
dominant_baseline = attr (AttrName "dominant-baseline") <<< printBaseline

-- TODO shouldn't this be 'classes' taking an (Array Classname), like the rest of Halogen?
class_ :: forall r i . String -> IProp (class :: String | r) i
class_ = attr (AttrName "class")

id :: forall r i . String -> IProp (id :: String | r) i
id = attr (AttrName "id")

markerWidth :: forall r i. Number -> IProp (markerWidth :: Number | r) i
markerWidth = attr (AttrName "markerWidth") <<< show

markerHeight :: forall r i. Number -> IProp (markerHeight :: Number | r) i
markerHeight = attr (AttrName "markerHeight") <<< show

refX :: forall r i. Number -> IProp (refX :: Number | r) i
refX = attr (AttrName "refX") <<< show

refY :: forall r i. Number -> IProp (refY :: Number | r) i
refY = attr (AttrName "refY") <<< show

orient :: forall r i. Orient -> IProp (orient :: String | r) i
orient = attr (AttrName "orient") <<< printOrient

markerUnits :: forall r i. MarkerUnit -> IProp (markerUnits :: String | r) i
markerUnits = attr (AttrName "markerUnits") <<< printMarkerUnit

strokeWidth :: forall r i. Number -> IProp (strokeWidth :: Number | r) i
strokeWidth = attr (AttrName "stroke-width") <<< show

markerEnd :: forall r i. String -> IProp (markerEnd :: String | r) i
markerEnd = attr (AttrName "marker-end")

--------------------------------------------------------------------------------

-- | https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/dur
data DurationF a = Duration (Maybe a) (Maybe a) (Maybe a) (Maybe a) -- ^ TODO hours minutes seconds millis

derive instance functorDurationF :: Functor DurationF

printDurationF :: forall a. Show a => DurationF a -> String
printDurationF (Duration h m s i) = f "h" h <> f "m" m <> f "s" s <> f "i" i
  where f u = maybe "" (\v -> show v <> u)

type Duration = DurationF Number

-- TODO derive Show instance for DurationF

printDuration :: Duration -> String
printDuration = printDurationF

-- TODO add other constructors
seconds :: Number -> Duration
seconds s = Duration Nothing Nothing (Just s) Nothing

data FillState = Freeze | Remove

printFillState :: FillState -> String
printFillState = case _ of
  Freeze -> "freeze"
  Remove -> "remove"

dur :: forall r i. Duration -> IProp (dur :: String | r) i
dur = attr (AttrName "dur") <<< printDuration

-- TODO ADT or free string?
attributeName :: forall r i. String -> IProp (attributeName :: String | r) i
attributeName = attr (AttrName "attributeName")

-- https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/from
from :: forall r i. String -> IProp (from :: String | r) i
from = attr (AttrName "from")

-- https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/to
to :: forall r i. String -> IProp (to :: String | r) i
to = attr (AttrName "to")

-- https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/begin
begin :: forall r i. String -> IProp (begin :: String | r) i
begin = attr (AttrName "begin")

repeatCount :: forall r i. Int -> IProp (repeatCount :: Int | r) i
repeatCount = attr (AttrName "repeatCount") <<< show

-- TODO this is just 'fill', but that functino is already specialised to Color in this module
fillAnim :: forall r i. FillState -> IProp (fill :: String | r) i
fillAnim = attr (AttrName "fill") <<< printFillState

-- TODO xlink:href seems to have some issues, among others around its namespace
xlinkHref :: forall r i. String -> IProp (xlinkHref :: String | r) i
-- xlinkHref = attr (AttrName "xlink:href")
-- xlinkHref = attrNS (Namespace "xlink") (AttrName "href")
xlinkHref = attrNS (Namespace "xlink") (AttrName "xlink:href")

-- TODO copied from `d`; adapt where needed
path :: forall r i . Array D -> IProp (path :: String | r) i
path = attr (AttrName "path") <<< joinWith " " <<< map printD
