module Text.SVG.Attribute

import Data.String
import Text.SVG.Tag
import Text.SVG.Types

%default total

public export
data SVGAttribute : {0 k : Type} -> (t : k) -> Type where
  Id    : String -> SVGAttribute t
  Str   : (name,val : String) -> SVGAttribute t
  Bool  : (name : String) -> Bool -> SVGAttribute t
  Empty : SVGAttribute t

export
displayAttribute : SVGAttribute t -> Maybe String
displayAttribute (Id va)        = Just #"id="\#{va}""#
displayAttribute (Str nm va)    = Just #"\#{nm}="\#{va}""#
displayAttribute (Bool nm True) = Just nm
displayAttribute (Bool _ False) = Nothing
displayAttribute Empty          = Nothing

export
displayAttributes : List (SVGAttribute t) -> String
displayAttributes = fastConcat . intersperse " " . mapMaybe displayAttribute

--------------------------------------------------------------------------------
-- Predefine Attributes
--------------------------------------------------------------------------------

export %inline
class : String -> SVGAttribute t
class = Str "class"

export %inline
classes : List String -> SVGAttribute t
classes = Str "class" . unwords

export %inline
d : List PathCmd -> SVGAttribute Path
d = Str "d" . unwords . map interpolate

export %inline
cx : (0 prf : HasCX t) => LengthOrPercentage -> SVGAttribute t
cx = Str "cx" . interpolate

export %inline
cy : (0 prf : HasCX t) => LengthOrPercentage -> SVGAttribute t
cy = Str "cy" . interpolate

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
fontSize : LengthOrPercentage -> SVGAttribute t
fontSize = Str "font-size" . interpolate

export %inline
textAnchor : String -> SVGAttribute Text
textAnchor = Str "text-anchor"

export %inline
fill : SVGColor -> SVGAttribute t
fill = Str "fill" . interpolate

export %inline
stroke : (0 p : HasStroke t) => SVGColor -> SVGAttribute t
stroke = Str "stroke" . interpolate

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
xmlns : String -> SVGAttribute SVG
xmlns = Str "xmlns"

export %inline
xmlns_2000 : SVGAttribute SVG
xmlns_2000 = xmlns "http://www.w3.org/2000/svg"
