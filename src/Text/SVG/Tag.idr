module Text.SVG.Tag

%default total

--------------------------------------------------------------------------------
-- Predicates
--------------------------------------------------------------------------------

||| Proof that the given element can have an `href` attribute
public export
data HasHref : (s : String) -> Type where
  HasHrefImage   : HasHref "image"
  HasHrefPattern : HasHref "pattern"
  HasHrefUse     : HasHref "use"

||| Proof that the given element can have a `cx` attribute
public export
data HasCX : (s : String) -> Type where
  HasCXCircle  : HasCX "circle"
  HasCXEllipse : HasCX "ellipse"

||| Proof that the given element can have a `cy` attribute
public export
data HasCY : (s : String) -> Type where
  HasCYCircle  : HasCY "circle"
  HasCYEllipse : HasCY "ellipse"

||| Proof that the given element can have an `rx` attribute
public export
data HasRX : (s : String) -> Type where
  HasRXEllipse : HasRX "ellipse"
  HasRXRect    : HasRX "rect"

||| Proof that the given element can have an `ry` attribute
public export
data HasRY : (s : String) -> Type where
  HasRYEllipse : HasRY "ellipse"
  HasRYRect    : HasRY "rect"

||| Proof that the given element can have an `x` attribute
public export
data HasX : (s : String) -> Type where
  HasXFilter  : HasX "filter"
  HasXImage   : HasX "image"
  HasXRect    : HasX "rect"
  HasXText    : HasX "text"

||| Proof that the given element can have a `y` attribute
public export
data HasY : (s : String) -> Type where
  HasYFilter  : HasY "filter"
  HasYImage   : HasY "image"
  HasYRect    : HasY "rect"
  HasYText    : HasY "text"

||| Proof that the given element can have a `height` attribute
public export
data HasHeight : (s : String) -> Type where
  HasHeightFilter  : HasHeight "filter"
  HasHeightImage   : HasHeight "image"
  HasHeightMask    : HasHeight "mask"
  HasHeightPath    : HasHeight "path"
  HasHeightPattern : HasHeight "pattern"
  HasHeightRect    : HasHeight "rect"
  HasHeightSVG     : HasHeight "svg"
  HasHeightUse     : HasHeight "use"

||| Proof that the given element can have a `fill` attribute
public export
data HasFill : (s : String) -> Type where
  HasFillCircle   : HasFill "circle"
  HasFillEllipse  : HasFill "ellipse"
  HasFillGroup    : HasFill "group"
  HasFillLine     : HasFill "line"
  HasFillPath     : HasFill "path"
  HasFillPolygon  : HasFill "polygon"
  HasFillPolyline : HasFill "polyline"
  HasFillRect     : HasFill "rect"
  HasFillSVG      : HasFill "svg"
  HasFillText     : HasFill "text"

||| Proof that the given element can have a `stroke` attribute
public export
data HasStroke : (s : String) -> Type where
  HasStrokeCircle   : HasStroke "circle"
  HasStrokeEllipse  : HasStroke "ellipse"
  HasStrokeGroup    : HasStroke "group"
  HasStrokeLine     : HasStroke "line"
  HasStrokePath     : HasStroke "path"
  HasStrokePolygon  : HasStroke "polygon"
  HasStrokePolyline : HasStroke "polyline"
  HasStrokeRect     : HasStroke "rect"
  HasStrokeSVG      : HasStroke "svg"
  HasStrokeText     : HasStroke "text"

||| Proof that the given element can have a `width` attribute
public export
data HasWidth : (s : String) -> Type where
  HasWidthFilter  : HasWidth "filter"
  HasWidthImage   : HasWidth "image"
  HasWidthMask    : HasWidth "mask"
  HasWidthPath    : HasWidth "path"
  HasWidthPattern : HasWidth "pattern"
  HasWidthRect    : HasWidth "rect"
  HasWidthSVG     : HasWidth "svg"
  HasWidthUse     : HasWidth "use"

||| Proof that the given element can have a `viewPort` attribute
public export
data HasViewBox : (s : String) -> Type where
  HasViewBoxMarker  : HasViewBox "marker"
  HasViewBoxPattern : HasViewBox "pattern"
  HasViewBoxSVG     : HasViewBox "svg"
  HasViewBoxSymbol  : HasViewBox "symbol"
  HasViewBoxView    : HasViewBox "view"

||| Proof that the given element can have a `points` attribute
public export
data HasPoints : (s : String) -> Type where
  HasPointsPolygon  : HasPoints "polygon"
  HasPointsPolyline  : HasPoints "polyline"

||| Proof that the given element displays some text
public export
data IsText : (s : String) -> Type where
  IsTextText  : IsText "text"
  IsTextTSpan : IsText "tspan"
  IsTextGroup : IsText "group"
  IsTextSVG   : IsText "svg"
