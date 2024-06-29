module Text.SVG.Tag

%default total

||| SVG elements with these tags currently get special
||| support in terms of type safety when creating
||| SVG trees.
public export
data SVGTag : String -> Type where
  Circle       : SVGTag "circle"
  Ellipse      : SVGTag "ellipse"
  Filter       : SVGTag "filter"
  Group        : SVGTag "g"
  Image        : SVGTag "image"
  Line         : SVGTag "line"
  Marker       : SVGTag "marker"
  Mask         : SVGTag "mask"
  Path         : SVGTag "path"
  Pattern      : SVGTag "pattern"
  Polygon      : SVGTag "polygon"
  Polyline     : SVGTag "polyline"
  Rect         : SVGTag "rect"
  SVG          : SVGTag "svg"
  Symbol       : SVGTag "symbol"
  Text         : SVGTag "text"
  Use          : SVGTag "use"
  View         : SVGTag "view"

--------------------------------------------------------------------------------
-- Predicates
--------------------------------------------------------------------------------

||| Proof that the given element can have an `href` attribute
public export
data HasHref : (t : SVGTag s) -> Type where
  HasHrefImage   : HasHref Image
  HasHrefPattern : HasHref Pattern
  HasHrefUse     : HasHref Use

||| Proof that the given element can have a `cx` attribute
public export
data HasCX : (t : SVGTag s) -> Type where
  HasCXCircle  : HasCX Circle
  HasCXEllipse : HasCX Ellipse

||| Proof that the given element can have a `cy` attribute
public export
data HasCY : (t : SVGTag s) -> Type where
  HasCYCircle  : HasCY Circle
  HasCYEllipse : HasCY Ellipse

||| Proof that the given element can have an `rx` attribute
public export
data HasRX : (t : SVGTag s) -> Type where
  HasRXEllipse : HasRX Ellipse
  HasRXRect    : HasRX Rect

||| Proof that the given element can have an `ry` attribute
public export
data HasRY : (t : SVGTag s) -> Type where
  HasRYEllipse : HasRY Ellipse
  HasRYRect    : HasRY Rect

||| Proof that the given element can have an `x` attribute
public export
data HasX : (t : SVGTag s) -> Type where
  HasXFilter  : HasX Filter
  HasXImage   : HasX Image
  HasXRect    : HasX Rect
  HasXText    : HasX Text

||| Proof that the given element can have a `y` attribute
public export
data HasY : (t : SVGTag s) -> Type where
  HasYFilter  : HasY Filter
  HasYImage   : HasY Image
  HasYRect    : HasY Rect
  HasYText    : HasY Text

||| Proof that the given element can have a `height` attribute
public export
data HasHeight : (t : SVGTag s) -> Type where
  HasHeightFilter  : HasHeight Filter
  HasHeightImage   : HasHeight Image
  HasHeightMask    : HasHeight Mask
  HasHeightPath    : HasHeight Path
  HasHeightPattern : HasHeight Pattern
  HasHeightRect    : HasHeight Rect
  HasHeightSVG     : HasHeight SVG
  HasHeightUse     : HasHeight Use

||| Proof that the given element can have a `fill` attribute
public export
data HasFill : (t : SVGTag s) -> Type where
  HasFillCircle   : HasFill Circle
  HasFillEllipse  : HasFill Ellipse
  HasFillGroup    : HasFill Group
  HasFillLine     : HasFill Line
  HasFillPath     : HasFill Path
  HasFillPolygon  : HasFill Polygon
  HasFillPolyline : HasFill Polyline
  HasFillRect     : HasFill Rect
  HasFillText     : HasFill Text

||| Proof that the given element can have a `stroke` attribute
public export
data HasStroke : (t : SVGTag s) -> Type where
  HasStrokeCircle   : HasStroke Circle
  HasStrokeEllipse  : HasStroke Ellipse
  HasStrokeGroup    : HasStroke Group
  HasStrokeLine     : HasStroke Line
  HasStrokePath     : HasStroke Path
  HasStrokePolygon  : HasStroke Polygon
  HasStrokePolyline : HasStroke Polyline
  HasStrokeRect     : HasStroke Rect
  HasStrokeText     : HasStroke Text

||| Proof that the given element can have a `width` attribute
public export
data HasWidth : (t : SVGTag s) -> Type where
  HasWidthFilter  : HasWidth Filter
  HasWidthImage   : HasWidth Image
  HasWidthMask    : HasWidth Mask
  HasWidthPath    : HasWidth Path
  HasWidthPattern : HasWidth Pattern
  HasWidthRect    : HasWidth Rect
  HasWidthSVG     : HasWidth SVG
  HasWidthUse     : HasWidth Use

||| Proof that the given element can have a `viewPort` attribute
public export
data HasViewBox : (t : SVGTag s) -> Type where
  HasViewBoxMarker  : HasViewBox Marker
  HasViewBoxPattern : HasViewBox Pattern
  HasViewBoxSVG     : HasViewBox SVG
  HasViewBoxSymbol  : HasViewBox Symbol
  HasViewBoxView    : HasViewBox View

||| Proof that the given element can have a `poins` attribute
public export
data HasPoints : (t : SVGTag s) -> Type where
  HasPointsPolygon  : HasPoints Polygon
  HasPointsPolyline  : HasPoints Polyline
