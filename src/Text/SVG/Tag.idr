module Text.SVG.Tag

%default total

public export
data SVGTag : String -> Type where
  Circle : SVGTag "circle"
  Rect   : SVGTag "rect"
  SVG    : SVGTag "svg"
  Text   : SVGTag "text"
