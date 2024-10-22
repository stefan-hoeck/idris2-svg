module Text.SVG.Attribute

import Data.String
import Text.SVG.Tag
import Text.SVG.Types

%default total

||| SVG Attributes, indexed by the SVG element's name.
|||
||| Implementation note: Numeric attributes get their own constructors.
||| This allows us to more efficiently set such attributes programmatically
||| at the DOM, avoiding unnecessary conversions to `String` and back.
public export
data SVGAttribute : (s : String) -> Type where
  Id     : {0 s : _} -> String -> SVGAttribute s
  Str    : {0 s : _} -> (name,val : String) -> SVGAttribute s
  Style  : {0 s : _} -> (name,val : String) -> SVGAttribute s
  Bool   : {0 s : _} -> (name : String) -> Bool -> SVGAttribute s
  LOP    : {0 s : _} -> (name : String) -> LengthOrPercentage -> SVGAttribute s
  Perc   : {0 s : _} -> (name : String) -> Percentage -> SVGAttribute s
  Pth    : {0 s : _} -> (name : String) -> List PathCmd -> SVGAttribute s
  Points : {0 s : _} -> (name : String) -> List Double -> SVGAttribute s
  Empty  : {0 s : _} -> SVGAttribute s

opac : String -> Percentage -> String
opac s p = #"\#{s}-opacity="\#{p}""#

displayAttribute : {0 s : _} -> SVGAttribute s -> String
displayAttribute (Id va)        = #"id="\#{va}""#
displayAttribute (Str nm va)    = #"\#{nm}="\#{va}""#
displayAttribute (LOP nm va)    = #"\#{nm}="\#{va}""#
displayAttribute (Perc nm va)   = #"\#{nm}="\#{va}""#
displayAttribute (Pth nm va)    = #"\#{nm}="\#{unwords $ map interpolate va}""#
displayAttribute (Points nm va) = #"\#{nm}="\#{unwords $ map renderDouble va}""#
displayAttribute (Bool nm True) = nm
displayAttribute (Bool _ False) = ""
displayAttribute Empty          = ""
displayAttribute (Style nm va)  = "\{nm}:\{va};"

export
displayAttributes : {0 s : _} -> List (SVGAttribute s) -> String
displayAttributes = go [<] [<]
  where
    go : SnocList String -> SnocList String -> List (SVGAttribute f) -> String
    go [<] attrs    [] = unwords (attrs <>> [])
    go styles attrs [] =
      unwords (attrs <>> [fastConcat $ "style=\"" :: (styles <>> ["\""])])
    go styles attrs (x::xs) =
      case x of
        Style n v => go (styles :< "\{n}:\{v};") attrs xs
        _         => go styles (attrs :< displayAttribute x) xs

--------------------------------------------------------------------------------
-- Predefine Attributes
--------------------------------------------------------------------------------

export %inline
d : List PathCmd -> SVGAttribute "path"
d = Pth "d"

export %inline
r : LengthOrPercentage -> SVGAttribute "circle"
r = LOP "r"

export %inline
x1 : LengthOrPercentage -> SVGAttribute "line"
x1 = LOP "x1"

export %inline
y1 : LengthOrPercentage -> SVGAttribute "line"
y1 = LOP "y1"

export %inline
x2 : LengthOrPercentage -> SVGAttribute "line"
x2 = LOP "x2"

export %inline
y2 : LengthOrPercentage -> SVGAttribute "line"
y2 = LOP "y2"

export %inline
xmlns : String -> SVGAttribute "svg"
xmlns = Str "xmlns"

export %inline
xmlns_2000 : SVGAttribute "svg"
xmlns_2000 = xmlns "http://www.w3.org/2000/svg"

parameters {0 s : String}

  svgCol : String -> SVGColor -> SVGAttribute s
  svgCol n c =
    case c of
      RGBA r g b a => Style n "\{RGB r g b};\{n}-opacity:\{show $ a.value / 100.0}"
      _            => Style n (interpolate c)

  export %inline
  transform : Transform -> SVGAttribute s
  transform = Str "transform" . interpolate

  export %inline
  transforms : List Transform -> SVGAttribute s
  transforms = Str "transform" . unwords . map interpolate

  export %inline
  translateX : (dx : Double) -> SVGAttribute s
  translateX dx = transform (Translate dx 0)

  export %inline
  translateY : (dy : Double) -> SVGAttribute s
  translateY dy = transform (Translate 0 dy)

  export %inline
  translate : (dx,dy : Double) -> SVGAttribute s
  translate dx dy = transform (Translate dx dy)

  export %inline
  rotate : (ang : Double) -> SVGAttribute s
  rotate ang = transform (Rotate ang)

  export %inline
  scaleX : (x : Double) -> SVGAttribute s
  scaleX x = transform (Scale x 1)

  export %inline
  scaleY : (y : Double) -> SVGAttribute s
  scaleY y = transform (Scale 1 y)

  export %inline
  scaleXY : (x,y : Double) -> SVGAttribute s
  scaleXY x y = transform (Scale x y)

  export %inline
  scale : (v : Double) -> SVGAttribute s
  scale v = transform (Scale v v)

  export %inline
  class : String -> SVGAttribute s
  class = Str "class"

  export %inline
  classes : List String -> SVGAttribute s
  classes = Str "class" . unwords

  export %inline
  cx : (0 prf : HasCX s) => LengthOrPercentage -> SVGAttribute s
  cx = LOP "cx"

  export %inline
  cy : (0 prf : HasCX s) => LengthOrPercentage -> SVGAttribute s
  cy = LOP "cy"

  export %inline
  rx : (0 prf : HasRX s) => LengthOrPercentage -> SVGAttribute s
  rx = LOP "rx"

  export %inline
  ry : (0 prf : HasRY s) => LengthOrPercentage -> SVGAttribute s
  ry = LOP "ry"

  export %inline
  x : (0 prf : HasX s) => LengthOrPercentage -> SVGAttribute s
  x = LOP "x"

  export %inline
  y : (0 prf : HasY s) => LengthOrPercentage -> SVGAttribute s
  y = LOP "y"

  export %inline
  fill : (0 p : HasFill s) => SVGColor -> SVGAttribute s
  fill = svgCol "fill"

  export %inline
  fillOpacity : (0 p : HasFill s) => Percentage -> SVGAttribute s
  fillOpacity = Style "fill-opacity" . interpolate

  export %inline
  stroke : (0 p : HasStroke s) => SVGColor -> SVGAttribute s
  stroke = svgCol "stroke"

  export %inline
  strokeLinecap : (0 p : HasStroke s) => StrokeLinecap -> SVGAttribute s
  strokeLinecap = Style "stroke-linecap" . interpolate

  export %inline
  strokeLinejoin : (0 p : HasStroke s) => StrokeLinejoin -> SVGAttribute s
  strokeLinejoin = Style "stroke-linejoin" . interpolate

  export %inline
  strokeOpacity : (0 p : HasStroke s) => Percentage -> SVGAttribute s
  strokeOpacity = Style "stroke-opacity" . interpolate

  export %inline
  width : (0 prf : HasWidth s) => LengthOrPercentage -> SVGAttribute s
  width = LOP "width"

  export %inline
  height : (0 prf : HasHeight s) => LengthOrPercentage -> SVGAttribute s
  height = LOP "height"

  export %inline
  strokeWidth : (0 p : HasStroke s) => LengthOrPercentage -> SVGAttribute s
  strokeWidth = Style "stroke-width" . interpolate

  export
  strokeDasharray : (0 p : HasStroke s) => List Double -> SVGAttribute s
  strokeDasharray = Style "stroke-dasharray" . unwords . map renderDouble

  export
  strokeDashoffset : (0 p : HasStroke s) => Double -> SVGAttribute s
  strokeDashoffset = Style "stroke-dashoffset" . renderDouble

  export
  points : (0 p : HasPoints s) => List Double -> SVGAttribute s
  points = Points "points"

  export
  viewBox :
       {auto 0 prf : HasViewBox s}
    -> (minX, minY, width, height : LengthOrPercentage)
    -> SVGAttribute s
  viewBox mx my w h = Str "viewBox" "\{mx} \{my} \{w} \{h}"

  export %inline
  dominantBaseline : (0 p : IsText s) => DominantBaseline -> SVGAttribute s
  dominantBaseline = Style "dominant-baseline" . interpolate

  export %inline
  textAnchor : (0 p : IsText s) => TextAnchor -> SVGAttribute s
  textAnchor = Style "text-anchor" . interpolate

  export %inline
  font : (0 p : IsText s) => String -> SVGAttribute s
  font = Style "font"

  export %inline
  fontFamily : (0 p : IsText s) => String -> SVGAttribute s
  fontFamily = Style "font-family"

  export %inline
  fontWeight : (0 p : IsText s) => FontWeight -> SVGAttribute s
  fontWeight = Style "font-weight" . interpolate

  export %inline
  lengthAdjust : (0 p : IsText s) => LengthAdjust -> SVGAttribute s
  lengthAdjust = Str "lengthAdjust" . interpolate

  export %inline
  textLength : (0 p : IsText s) => LengthOrPercentage -> SVGAttribute s
  textLength = LOP "textLength"

  export %inline
  dx : (0 p : IsText s) => LengthOrPercentage -> SVGAttribute s
  dx = LOP "dx"

  export %inline
  dy : (0 p : IsText s) => LengthOrPercentage -> SVGAttribute s
  dy = LOP "dy"

  export %inline
  fontSize : (0 p : IsText s) => LengthOrPercentage -> SVGAttribute s
  fontSize = Style "font-size" . interpolate
