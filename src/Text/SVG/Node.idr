module Text.SVG.Node

import Data.String
import Text.SVG.Attribute as A
import Text.SVG.Tag
import Text.SVG.Types

%default total

public export
data SVGNode : Type where
  El    :
       {tag : _}
    -> (0 t : SVGTag tag)
    -> List (SVGAttribute t)
    -> List SVGNode
    -> SVGNode
  Txt   : String -> SVGNode
  Raw   : String -> SVGNode
  Empty : SVGNode

export %inline
el : {s : _} -> (0 t : SVGTag s) -> List (SVGAttribute t) -> SVGNode
el t as = El t as []

export %inline
circle : List (SVGAttribute Circle) -> SVGNode
circle = el _

export %inline
ellipse : List (SVGAttribute Ellipse) -> SVGNode
ellipse = el _

export %inline
g : List (SVGAttribute Group) -> List SVGNode -> SVGNode
g = El _

export %inline
image : List (SVGAttribute Image) -> SVGNode
image = el _

export %inline
line : List (SVGAttribute Tag.Line) -> SVGNode
line = el _

export %inline
mask : List (SVGAttribute Image) -> List SVGNode -> SVGNode
mask = El _

export %inline
rect : List (SVGAttribute Rect) -> SVGNode
rect = el _

export %inline
path : List (SVGAttribute Path) -> SVGNode
path = el _

export %inline
polygon : List (SVGAttribute Polygon) -> SVGNode
polygon = el _

export %inline
polyline : List (SVGAttribute Polyline) -> SVGNode
polyline = el _

export %inline
svg : List (SVGAttribute SVG) -> List SVGNode -> SVGNode
svg = El _

export %inline
symbol : List (SVGAttribute Symbol) -> List SVGNode -> SVGNode
symbol = El _

export %inline
text1 : List (SVGAttribute Text) -> String -> SVGNode
text1 as s = El _ as [Txt s]

export %inline
text : List (SVGAttribute Text) -> List SVGNode -> SVGNode
text as = El _ as

export %inline
tspan : List (SVGAttribute TSpan) -> List SVGNode -> SVGNode
tspan as = El _ as

export %inline
use : List (SVGAttribute Use) -> SVGNode
use = el _

export %inline
view : List (SVGAttribute Tag.View) -> SVGNode
view = el _

--------------------------------------------------------------------------------
-- Render
--------------------------------------------------------------------------------

export
escape : String -> String
escape = fastConcat . map esc . unpack

  where
    esc : Char -> String
    esc '<'          = "&lt;"
    esc '>'          = "&gt;"
    esc '&'          = "&amp;"
    esc '"'          = "&quot;"
    esc '\''         = "&#x27"
    esc '\n'         = "\n"
    esc '\r'         = "\r"
    esc '\t'         = "\t"
    esc c            = if c < ' ' then "" else singleton c

attrs : {0 t : _} -> List (SVGAttribute t) -> String
attrs as = let s = displayAttributes as in if null s then "" else " " ++ s

export
render : SVGNode -> String
render n = case n of
  Raw x             => x
  Txt x             => escape x
  El {tag} _ as []  => "<\{tag}\{attrs as}/>"
  El {tag} _ as ns  => "<\{tag}\{attrs as}>\{go [<] ns}</\{tag}>"
  Empty             => ""

  where
    go : SnocList String -> List SVGNode -> String
    go ss (n :: ns) = go (ss :< render n) ns
    go ss []        = concat $ ss <>> []

export
renderMany : List SVGNode -> String
renderMany = fastConcat . map render
