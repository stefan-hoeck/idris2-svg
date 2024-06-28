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
cx : LengthOrPercentage -> SVGAttribute Circle
cx = Str "cx" . interpolate

export %inline
cy : LengthOrPercentage -> SVGAttribute Circle
cy = Str "cy" . interpolate

export %inline
r : LengthOrPercentage -> SVGAttribute Circle
r = Str "r" . interpolate

export %inline
x : LengthOrPercentage -> SVGAttribute t
x = Str "x" . interpolate

export %inline
y : LengthOrPercentage -> SVGAttribute t
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
stroke : SVGColor -> SVGAttribute t
stroke = Str "stroke" . interpolate

export %inline
width : LengthOrPercentage -> SVGAttribute t
width = Str "width" . interpolate

export %inline
height : LengthOrPercentage -> SVGAttribute t
height = Str "height" . interpolate

export %inline
xmlns : String -> SVGAttribute SVG
xmlns = Str "xmlns"

export %inline
xmlns_2000 : SVGAttribute SVG
xmlns_2000 = xmlns "http://www.w3.org/2000/svg"
