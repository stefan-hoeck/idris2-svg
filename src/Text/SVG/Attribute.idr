module Text.SVG.Attribute

import Data.String
import Text.SVG.Tag
import Text.SVG.Types

%default total

public export
data SVGAttribute : {0 k : Type} -> (t : k) -> Type where
  Id    : {0 t : _} -> String -> SVGAttribute t
  Str   : {0 t : _} -> (name,val : String) -> SVGAttribute t
  Bool  : {0 t : _} -> (name : String) -> Bool -> SVGAttribute t
  Empty : {0 t : _} -> SVGAttribute t

export
displayAttribute : {0 t : _} -> SVGAttribute t -> Maybe String
displayAttribute (Id va)        = Just #"id="\#{va}""#
displayAttribute (Str nm va)    = Just #"\#{nm}="\#{va}""#
displayAttribute (Bool nm True) = Just nm
displayAttribute (Bool _ False) = Nothing
displayAttribute Empty          = Nothing

export
displayAttributes : {0 t : _} -> List (SVGAttribute t) -> String
displayAttributes = fastConcat . intersperse " " . mapMaybe displayAttribute

--------------------------------------------------------------------------------
-- Predefine Attributes
--------------------------------------------------------------------------------

export %inline
d : List PathCmd -> SVGAttribute Path
d = Str "d" . unwords . map interpolate

export %inline
r : LengthOrPercentage -> SVGAttribute Circle
r = Str "r" . interpolate

export %inline
x1 : LengthOrPercentage -> SVGAttribute Tag.Line
x1 = Str "x1" . interpolate

export %inline
y1 : LengthOrPercentage -> SVGAttribute Tag.Line
y1 = Str "y1" . interpolate

export %inline
x2 : LengthOrPercentage -> SVGAttribute Tag.Line
x2 = Str "x2" . interpolate

export %inline
y2 : LengthOrPercentage -> SVGAttribute Tag.Line
y2 = Str "y2" . interpolate

export %inline
xmlns : String -> SVGAttribute SVG
xmlns = Str "xmlns"

export %inline
xmlns_2000 : SVGAttribute SVG
xmlns_2000 = xmlns "http://www.w3.org/2000/svg"

parameters {0 s : String}
           {0 t : SVGTag s}

  export %inline
  transform : Transform -> SVGAttribute t
  transform = Str "transform" . interpolate

  export %inline
  transforms : List Transform -> SVGAttribute t
  transforms = Str "transform" . unwords . map interpolate

  export %inline
  translateX : (dx : Number) -> SVGAttribute t
  translateX dx = transform (Translate dx 0)

  export %inline
  translateY : (dy : Number) -> SVGAttribute t
  translateY dy = transform (Translate 0 dy)

  export %inline
  translate : (dx,dy : Number) -> SVGAttribute t
  translate dx dy = transform (Translate dx dy)

  export %inline
  rotate : (ang : Number) -> SVGAttribute t
  rotate ang = transform (Rotate ang)

  export %inline
  scaleX : (x : Number) -> SVGAttribute t
  scaleX x = transform (Scale x 1)

  export %inline
  scaleY : (y : Number) -> SVGAttribute t
  scaleY y = transform (Scale 1 y)

  export %inline
  scaleXY : (x,y : Number) -> SVGAttribute t
  scaleXY x y = transform (Scale x y)

  export %inline
  scale : (v : Number) -> SVGAttribute t
  scale v = transform (Scale v v)

  export %inline
  class : String -> SVGAttribute t
  class = Str "class"

  export %inline
  classes : List String -> SVGAttribute t
  classes = Str "class" . unwords

  export %inline
  cx : (0 prf : HasCX t) => LengthOrPercentage -> SVGAttribute t
  cx = Str "cx" . interpolate

  export %inline
  cy : (0 prf : HasCX t) => LengthOrPercentage -> SVGAttribute t
  cy = Str "cy" . interpolate

  export %inline
  rx : (0 prf : HasRX t) => LengthOrPercentage -> SVGAttribute t
  rx = Str "rx" . interpolate

  export %inline
  ry : (0 prf : HasRY t) => LengthOrPercentage -> SVGAttribute t
  ry = Str "ry" . interpolate

  export %inline
  x : (0 prf : HasX t) => LengthOrPercentage -> SVGAttribute t
  x = Str "x" . interpolate

  export %inline
  y : (0 prf : HasY t) => LengthOrPercentage -> SVGAttribute t
  y = Str "y" . interpolate

  export %inline
  fill : (0 p : HasFill t) => SVGColor -> SVGAttribute t
  fill = Str "fill" . interpolate

  export %inline
  fillOpacity : (0 p : HasFill t) => Percentage -> SVGAttribute t
  fillOpacity = Str "fill-opacity" . interpolate

  export %inline
  stroke : (0 p : HasStroke t) => SVGColor -> SVGAttribute t
  stroke = Str "stroke" . interpolate

  export %inline
  strokeLinecap : (0 p : HasStroke t) => StrokeLinecap -> SVGAttribute t
  strokeLinecap = Str "stroke-linecap" . interpolate

  export %inline
  strokeLinejoin : (0 p : HasStroke t) => StrokeLinejoin -> SVGAttribute t
  strokeLinejoin = Str "stroke-linejoin" . interpolate

  export %inline
  strokeOpacity : (0 p : HasStroke t) => Percentage -> SVGAttribute t
  strokeOpacity = Str "stroke-opacity" . interpolate

  export %inline
  width : (0 prf : HasWidth t) => LengthOrPercentage -> SVGAttribute t
  width = Str "width" . interpolate

  export %inline
  height : (0 prf : HasHeight t) => LengthOrPercentage -> SVGAttribute t
  height = Str "height" . interpolate

  export %inline
  strokeWidth : (0 p : HasStroke t) => LengthOrPercentage -> SVGAttribute t
  strokeWidth = Str "stroke-width" . interpolate

  export
  points : (0 p : HasPoints t) => List Number -> SVGAttribute t
  points = Str "points" . unwords . map interpolate

  export
  viewBox :
       {auto 0 prf : HasViewBox t}
    -> (minX, minY, width, height : LengthOrPercentage)
    -> SVGAttribute t
  viewBox mx my w h = Str "viewBox" "\{mx} \{my} \{w} \{h}"

  export %inline
  dominantBaseline : (0 p : IsText t) => DominantBaseline -> SVGAttribute t
  dominantBaseline = Str "dominant-baseline" . interpolate

  export %inline
  textAnchor : (0 p : IsText t) => TextAnchor -> SVGAttribute t
  textAnchor = Str "text-anchor" . interpolate

  export %inline
  font : (0 p : IsText t) => String -> SVGAttribute t
  font = Str "font"

  export %inline
  fontFamily : (0 p : IsText t) => String -> SVGAttribute t
  fontFamily = Str "font-family"

  export %inline
  fontWeight : (0 p : IsText t) => FontWeight -> SVGAttribute t
  fontWeight = Str "font-weight" . interpolate

  export %inline
  lengthAdjust : (0 p : IsText t) => LengthAdjust -> SVGAttribute t
  lengthAdjust = Str "lengthAdjust" . interpolate

  export %inline
  textLength : (0 p : IsText t) => LengthOrPercentage -> SVGAttribute t
  textLength = Str "textLength" . interpolate

  export %inline
  dx : (0 p : IsText t) => LengthOrPercentage -> SVGAttribute t
  dx = Str "dx" . interpolate

  export %inline
  dy : (0 p : IsText t) => LengthOrPercentage -> SVGAttribute t
  dy = Str "dy" . interpolate

  export %inline
  fontSize : (0 p : IsText t) => LengthOrPercentage -> SVGAttribute t
  fontSize = Str "font-size" . interpolate
