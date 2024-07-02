module Text.SVG.Types

import Data.Bits
import Derive.Prelude
import public Data.Refined

%default total
%language ElabReflection

||| Prints a floating point number, removing the trailing `".0"`
||| in case it's an integer.
export
renderDouble : Double -> String
renderDouble d =
  let s := show d
   in case [<] <>< unpack s of
        (h :< '.' :< '0') => pack (h <>> [])
        _                 => s

%inline
Interpolation Double where interpolate = renderDouble

public export
data SVGAngle : Type where
  Deg  : Double -> SVGAngle
  Rad  : Double -> SVGAngle
  Grad : Double -> SVGAngle

export
Interpolation SVGAngle where
  interpolate (Deg x)  = renderDouble x ++ "deg"
  interpolate (Rad x)  = renderDouble x ++ "rad"
  interpolate (Grad x) = renderDouble x ++ "grad"

export %inline
deg : Cast SVGAngle a => Double -> a
deg = cast . Deg

export %inline
rad : Cast SVGAngle a => Double -> a
rad = cast . Rad

export %inline
grad : Cast SVGAngle a => Double -> a
grad = cast . Grad

export %inline
(.deg) : Cast SVGAngle a => Double -> a
(.deg) = cast . Deg

export %inline
(.rad) : Cast SVGAngle a => Double -> a
(.rad) = cast . Rad

export %inline
(.grad) : Cast SVGAngle a => Double -> a
(.grad) = cast . Grad

--------------------------------------------------------------------------------
--          Percentage
--------------------------------------------------------------------------------

public export
IsPercentage : Double -> Bool
IsPercentage x = 0 <= x && x <= 100

||| A floating point percentage value in the the
||| range [0,100].
public export
record Percentage where
  constructor MkPercentage
  value : Double
  {auto 0 prf : Holds IsPercentage value}

%runElab derive "Percentage" [Show,Eq,Ord]

export
Interpolation Percentage where
  interpolate (MkPercentage v) = renderDouble v ++ "%"

||| Convenience function for creating percentages with little
||| syntactic overhead.
|||
||| ```idris example
||| perc 12
||| ```
export %inline
perc :
     {auto _ : Cast Percentage a}
  -> (v : Double)
  -> {auto 0 prf : Holds IsPercentage v}
  -> a
perc v = cast $ MkPercentage v

||| Convenience function for creating percentages with little
||| syntactic overhead.
|||
||| ```idris example
||| 12.perc
||| ```
export %inline
(.perc) :
     {auto _ : Cast Percentage a}
  -> (v : Double)
  -> {auto 0 prf : Holds IsPercentage v}
  -> a
(.perc) v = cast $ MkPercentage v

--------------------------------------------------------------------------------
--          SVGColor
--------------------------------------------------------------------------------

hexc : Bits8 -> Char
hexc 0  = '0'
hexc 1  = '1'
hexc 2  = '2'
hexc 3  = '3'
hexc 4  = '4'
hexc 5  = '5'
hexc 6  = '6'
hexc 7  = '7'
hexc 8  = '8'
hexc 9  = '9'
hexc 10 = 'a'
hexc 11 = 'b'
hexc 12 = 'c'
hexc 13 = 'd'
hexc 14 = 'e'
hexc _  = 'f'

hex : Bits8 -> List Char
hex b = [hexc $ shiftR b 4, hexc $ b .&. 0xf]

public export
data SVGColor : Type where
  RGB  : (red,green,blue : Bits8) -> SVGColor
  RGBA : (red,green,blue : Bits8) -> Percentage -> SVGColor
  Key  : String -> SVGColor

export
Interpolation SVGColor where
  interpolate (RGB r g b)    = fastPack $ '#' :: hex r ++ hex g ++ hex b
  interpolate (RGBA r g b a) = "rgba(\{show r} \{show g} \{show b} \{a})"
  interpolate (Key s)        = s

export
Show SVGColor where show = interpolate

--------------------------------------------------------------------------------
--          Length
--------------------------------------------------------------------------------

public export
data Length : Type where
  U        : Double -> Length
  Pt       : Double -> Length
  Px       : Double -> Length
  Mm       : Double -> Length
  Cm       : Double -> Length

export
Interpolation Length where
  interpolate (U x)  =  renderDouble x
  interpolate (Pt x)  = renderDouble x ++ "pt"
  interpolate (Px x)  = renderDouble x ++ "px"
  interpolate (Mm x)  = renderDouble x ++ "mm"
  interpolate (Cm x)  = renderDouble x ++ "cm"

export %inline
u : Cast Length a => Double -> a
u = cast . U

export %inline
mm : Cast Length a => Double -> a
mm = cast . Mm

export %inline
cm : Cast Length a => Double -> a
cm = cast . Cm

export %inline
px : Cast Length a => Double -> a
px = cast . Px

export %inline
pt : Cast Length a => Double -> a
pt = cast . Pt

export %inline
(.u) : Cast Length a => Double -> a
(.u) = cast . U

export %inline
(.mm) : Cast Length a => Double -> a
(.mm) = cast . Mm

export %inline
(.cm) : Cast Length a => Double -> a
(.cm) = cast . Cm

export %inline
(.px) : Cast Length a => Double -> a
(.px) = cast . Px

export %inline
(.pt) : Cast Length a => Double -> a
(.pt) = cast . Pt

--------------------------------------------------------------------------------
--          Length or Percent
--------------------------------------------------------------------------------

public export
data LengthOrPercentage : Type where
  Len : Length -> LengthOrPercentage
  Per : Percentage -> LengthOrPercentage

export
Interpolation LengthOrPercentage where
  interpolate (Len x) = interpolate x
  interpolate (Per x) = interpolate x

export %inline
Cast Percentage LengthOrPercentage where cast = Per

export %inline
Cast Length LengthOrPercentage where cast = Len

--------------------------------------------------------------------------------
--          Paths
--------------------------------------------------------------------------------

public export
data PathCmd : Type where
  Move  : (rel : Bool) -> (x,y : Double) -> PathCmd
  Line  : (rel : Bool) -> (x,y : Double) -> PathCmd
  Horiz : (rel : Bool) -> (x : Double) -> PathCmd
  Vert  : (rel : Bool) -> (y : Double) -> PathCmd
  Z     : PathCmd
  Quadr : (rel : Bool) -> (x1,y1,x,y : Double) -> PathCmd
  QSucc : (rel : Bool) -> (x,y : Double) -> PathCmd
  Cubic : (rel : Bool) -> (x1,y1,x2,y2,x,y : Double) -> PathCmd
  CSucc : (rel : Bool) -> (x2,y2,x,y : Double) -> PathCmd
  Arc   :
       (rel : Bool)
    -> (rx,ry,rot : Double)
    -> (largeArc,sweep : Bool)
    -> (x,y : Double)
    -> PathCmd

letter : Bool -> String -> String -> String
letter False u l = u
letter True  u l = l

digit : Bool -> String
digit False = "0"
digit True  = "1"

export
Interpolation PathCmd where
  interpolate (Move rel x y) = letter rel "M" "m" ++ "\{x} \{y}"
  interpolate (Line rel x y) = letter rel "L" "l" ++ "\{x} \{y}"
  interpolate (Horiz rel x)  = letter rel "H" "h" ++ "\{x}"
  interpolate (Vert rel y)   = letter rel "V" "v" ++ "\{y}"
  interpolate Z              = "Z"
  interpolate (QSucc rel x y) =
    letter rel "T" "t" ++ "\{x} \{y}"
  interpolate (CSucc rel x2 y2 x y) =
    letter rel "S" "s" ++ "\{x2} \{y2},\{x} \{y}"
  interpolate (Quadr rel x1 y1 x y) =
    letter rel "Q" "q" ++ "\{x1} \{y1},\{x} \{y}"
  interpolate (Cubic rel x1 y1 x2 y2 x y) =
    letter rel "C" "c" ++ "\{x1} \{y1},\{x2} \{y2},\{x} \{y}"
  interpolate (Arc rel rx ry rot l s x y) =
    letter rel "A" "a" ++ "\{rx} \{ry} \{rot} \{digit l} \{digit s} \{x} \{y}"

namespace Path
  export %inline
  M : (x,y : Double) -> PathCmd
  M = Move False

  export %inline
  m : (x,y : Double) -> PathCmd
  m = Move True

  export %inline
  L : (x,y : Double) -> PathCmd
  L = Line False

  export %inline
  l : (x,y : Double) -> PathCmd
  l = Line True

  export %inline
  H : (x : Double) -> PathCmd
  H = Horiz False

  export %inline
  h : (x : Double) -> PathCmd
  h = Horiz True

  export %inline
  V : (x : Double) -> PathCmd
  V = Vert False

  export %inline
  v : (x : Double) -> PathCmd
  v = Vert True

  export %inline
  S : (x2,y2,x,y : Double) -> PathCmd
  S = CSucc False

  export %inline
  s : (x2,y2,x,y : Double) -> PathCmd
  s = CSucc True

  export %inline
  C : (x1,y1,x2,y2,x,y : Double) -> PathCmd
  C = Cubic False

  export %inline
  c : (x1,y1,x2,y2,x,y : Double) -> PathCmd
  c = Cubic True

  export %inline
  T : (x,y : Double) -> PathCmd
  T = QSucc False

  export %inline
  t : (x,y : Double) -> PathCmd
  t = QSucc True

  export %inline
  Q : (x1,y1,x,y : Double) -> PathCmd
  Q = Quadr False

  export %inline
  q : (x1,y1,x,y : Double) -> PathCmd
  q = Quadr True

  export %inline
  A   :
       (rx,ry,rot : Double)
    -> (largeArc,sweep : Bool)
    -> (x,y : Double)
    -> PathCmd
  A = Arc False

  export %inline
  a   :
       (rx,ry,rot : Double)
    -> (largeArc,sweep : Bool)
    -> (x,y : Double)
    -> PathCmd
  a = Arc True

--------------------------------------------------------------------------------
--          Strokes
--------------------------------------------------------------------------------

public export
data StrokeLinecap = Butt | Round | Square

export
Interpolation StrokeLinecap where
  interpolate Butt   = "butt"
  interpolate Round  = "round"
  interpolate Square = "square"

namespace StrokeLinejoin
  public export
  data StrokeLinejoin = Miter | Round | Bevel

  export
  Interpolation StrokeLinejoin where
    interpolate Miter   = "miter"
    interpolate Round   = "round"
    interpolate Bevel   = "bevel"

--------------------------------------------------------------------------------
--          Text
--------------------------------------------------------------------------------

public export
data TextAnchor = Start | Middle | End

export
Interpolation TextAnchor where
  interpolate Start  = "start"
  interpolate Middle = "middle"
  interpolate End    = "end"

namespace DominantBaselie
  public export
  data DominantBaseline : Type where
    Auto           : DominantBaseline
    Ideographic    : DominantBaseline
    Alphabetic     : DominantBaseline
    Hanging        : DominantBaseline
    Mathematical   : DominantBaseline
    Middle         : DominantBaseline
    Central        : DominantBaseline
    TextAfterEdge  : DominantBaseline
    TextBeforeEdge : DominantBaseline
    TextBottom     : DominantBaseline
    TextTop        : DominantBaseline

  export
  Interpolation DominantBaseline where
    interpolate Auto           = "auto"
    interpolate Ideographic    = "ideographic"
    interpolate Alphabetic     = "alphabetic"
    interpolate Hanging        = "hanging"
    interpolate Mathematical   = "mathematical"
    interpolate Middle         = "middle"
    interpolate Central        = "central"
    interpolate TextAfterEdge  = "text-after-edge"
    interpolate TextBeforeEdge = "text-before-edge"
    interpolate TextBottom     = "text-bottom"
    interpolate TextTop        = "text-top"

public export
data FontWeight : Type where
  Normal  : FontWeight
  Bold    : FontWeight
  Bolder  : FontWeight
  Lighter : FontWeight
  Val     : Double -> FontWeight

export
Interpolation FontWeight where
  interpolate Normal  = "normal"
  interpolate Bold    = "bold"
  interpolate Bolder  = "bolder"
  interpolate Lighter = "lighter"
  interpolate (Val x) = renderDouble x

export
data LengthAdjust = Spacing | SpacingAndGlyphs

export
Interpolation LengthAdjust where
  interpolate Spacing          = "spacing"
  interpolate SpacingAndGlyphs = "spacingAndGlyphs"

--------------------------------------------------------------------------------
--          Transformations
--------------------------------------------------------------------------------

public export
data Transform : Type where
  Translate : (dx,dy : Double) -> Transform
  Rotate    : (angle : Double) -> Transform
  Scale     : (x,y   : Double) -> Transform
  Matrix    : (a,b,c,d,e,f : Double) -> Transform

export
Interpolation Transform where
  interpolate (Translate dx dy)    = "translate(\{dx},\{dy})"
  interpolate (Rotate angle)       = "rotate(\{angle})"
  interpolate (Scale x y)          = "scale(\{x},\{y})"
  interpolate (Matrix a b c d e f) = "matrix(\{a},\{b},\{c},\{d},\{e},\{f})"

--------------------------------------------------------------------------------
--          X11 Colors (https://www.w3.org/TR/css3-color/#svg-color)
--------------------------------------------------------------------------------

export
none : SVGColor
none = Key "none"

export
aliceblue : SVGColor
aliceblue = Key "aliceblue"

export
antiquewhite : SVGColor
antiquewhite = Key "antiquewhite"

export
aqua : SVGColor
aqua = Key "aqua"

export
aquamarine : SVGColor
aquamarine = Key "aquamarine"

export
azure : SVGColor
azure = Key "azure"

export
beige : SVGColor
beige = Key "beige"

export
bisque : SVGColor
bisque = Key "bisque"

export
blanchedalmond : SVGColor
blanchedalmond = Key "blanchedalmond"

export
black : SVGColor
black = Key "black"

export
blue : SVGColor
blue = Key "blue"

export
blueviolet : SVGColor
blueviolet = Key "blueviolet"

export
brown : SVGColor
brown = Key "brown"

export
burlywood : SVGColor
burlywood = Key "burlywood"

export
cadetblue : SVGColor
cadetblue = Key "cadetblue"

export
chartreuse : SVGColor
chartreuse = Key "chartreuse"

export
chocolate : SVGColor
chocolate = Key "chocolate"

export
coral : SVGColor
coral = Key "coral"

export
cornflowerblue : SVGColor
cornflowerblue = Key "cornflowerblue"

export
cornsilk : SVGColor
cornsilk = Key "cornsilk"

export
crimson : SVGColor
crimson = Key "crimson"

export
cyan : SVGColor
cyan = Key "cyan"

export
darkblue : SVGColor
darkblue = Key "darkblue"

export
darkcyan : SVGColor
darkcyan = Key "darkcyan"

export
darkgoldenrod : SVGColor
darkgoldenrod = Key "darkgoldenrod"

export
darkgray : SVGColor
darkgray = Key "darkgray"

export
darkgreen : SVGColor
darkgreen = Key "darkgreen"

export
darkgrey : SVGColor
darkgrey = Key "darkgrey"

export
darkkhaki : SVGColor
darkkhaki = Key "darkkhaki"

export
darkmagenta : SVGColor
darkmagenta = Key "darkmagenta"

export
darkolivegreen : SVGColor
darkolivegreen = Key "darkolivegreen"

export
darkorange : SVGColor
darkorange = Key "darkorange"

export
darkorchid : SVGColor
darkorchid = Key "darkorchid"

export
darkred : SVGColor
darkred = Key "darkred"

export
darksalmon : SVGColor
darksalmon = Key "darksalmon"

export
darkseagreen : SVGColor
darkseagreen = Key "darkseagreen"

export
darkslateblue : SVGColor
darkslateblue = Key "darkslateblue"

export
darkslategray : SVGColor
darkslategray = Key "darkslategray"

export
darkslategrey : SVGColor
darkslategrey = Key "darkslategrey"

export
darkturquoise : SVGColor
darkturquoise = Key "darkturquoise"

export
darkviolet : SVGColor
darkviolet = Key "darkviolet"

export
deeppink : SVGColor
deeppink = Key "deeppink"

export
deepskyblue : SVGColor
deepskyblue = Key "deepskyblue"

export
dimgray : SVGColor
dimgray = Key "dimgray"

export
dimgrey : SVGColor
dimgrey = Key "dimgrey"

export
dodgerblue : SVGColor
dodgerblue = Key "dodgerblue"

export
firebrick : SVGColor
firebrick = Key "firebrick"

export
floralwhite : SVGColor
floralwhite = Key "floralwhite"

export
forestgreen : SVGColor
forestgreen = Key "forestgreen"

export
fuchsia : SVGColor
fuchsia = Key "fuchsia"

export
gainsboro : SVGColor
gainsboro = Key "gainsboro"

export
ghostwhite : SVGColor
ghostwhite = Key "ghostwhite"

export
gold : SVGColor
gold = Key "gold"

export
goldenrod : SVGColor
goldenrod = Key "goldenrod"

export
gray : SVGColor
gray = Key "gray"

export
green : SVGColor
green = Key "green"

export
greenyellow : SVGColor
greenyellow = Key "greenyellow"

export
grey : SVGColor
grey = Key "grey"

export
honeydew : SVGColor
honeydew = Key "honeydew"

export
hotpink : SVGColor
hotpink = Key "hotpink"

export
indianred : SVGColor
indianred = Key "indianred"

export
indigo : SVGColor
indigo = Key "indigo"

export
ivory : SVGColor
ivory = Key "ivory"

export
khaki : SVGColor
khaki = Key "khaki"

export
lavender : SVGColor
lavender = Key "lavender"

export
lavenderblush : SVGColor
lavenderblush = Key "lavenderblush"

export
lawngreen : SVGColor
lawngreen = Key "lawngreen"

export
lemonchiffon : SVGColor
lemonchiffon = Key "lemonchiffon"

export
lightblue : SVGColor
lightblue = Key "lightblue"

export
lightcoral : SVGColor
lightcoral = Key "lightcoral"

export
lightcyan : SVGColor
lightcyan = Key "lightcyan"

export
lightgoldenrodyellow : SVGColor
lightgoldenrodyellow = Key "lightgoldenrodyellow"

export
lightgray : SVGColor
lightgray = Key "lightgray"

export
lightgreen : SVGColor
lightgreen = Key "lightgreen"

export
lightgrey : SVGColor
lightgrey = Key "lightgrey"

export
lightpink : SVGColor
lightpink = Key "lightpink"

export
lightsalmon : SVGColor
lightsalmon = Key "lightsalmon"

export
lightseagreen : SVGColor
lightseagreen = Key "lightseagreen"

export
lightskyblue : SVGColor
lightskyblue = Key "lightskyblue"

export
lightslategray : SVGColor
lightslategray = Key "lightslategray"

export
lightslategrey : SVGColor
lightslategrey = Key "lightslategrey"

export
lightsteelblue : SVGColor
lightsteelblue = Key "lightsteelblue"

export
lightyellow : SVGColor
lightyellow = Key "lightyellow"

export
lime : SVGColor
lime = Key "lime"

export
limegreen : SVGColor
limegreen = Key "limegreen"

export
linen : SVGColor
linen = Key "linen"

export
magenta : SVGColor
magenta = Key "magenta"

export
maroon : SVGColor
maroon = Key "maroon"

export
mediumaquamarine : SVGColor
mediumaquamarine = Key "mediumaquamarine"

export
mediumblue : SVGColor
mediumblue = Key "mediumblue"

export
mediumorchid : SVGColor
mediumorchid = Key "mediumorchid"

export
mediumpurple : SVGColor
mediumpurple = Key "mediumpurple"

export
mediumseagreen : SVGColor
mediumseagreen = Key "mediumseagreen"

export
mediumslateblue : SVGColor
mediumslateblue = Key "mediumslateblue"

export
mediumspringgreen : SVGColor
mediumspringgreen = Key "mediumspringgreen"

export
mediumturquoise : SVGColor
mediumturquoise = Key "mediumturquoise"

export
mediumvioletred : SVGColor
mediumvioletred = Key "mediumvioletred"

export
midnightblue : SVGColor
midnightblue = Key "midnightblue"

export
mintcream : SVGColor
mintcream = Key "mintcream"

export
mistyrose : SVGColor
mistyrose = Key "mistyrose"

export
moccasin : SVGColor
moccasin = Key "moccasin"

export
navajowhite : SVGColor
navajowhite = Key "navajowhite"

export
navy : SVGColor
navy = Key "navy"

export
oldlace : SVGColor
oldlace = Key "oldlace"

export
olive : SVGColor
olive = Key "olive"

export
olivedrab : SVGColor
olivedrab = Key "olivedrab"

export
orange : SVGColor
orange = Key "orange"

export
orangered : SVGColor
orangered = Key "orangered"

export
orchid : SVGColor
orchid = Key "orchid"

export
palegoldenrod : SVGColor
palegoldenrod = Key "palegoldenrod"

export
palegreen : SVGColor
palegreen = Key "palegreen"

export
paleturquoise : SVGColor
paleturquoise = Key "paleturquoise"

export
palevioletred : SVGColor
palevioletred = Key "palevioletred"

export
papayawhip : SVGColor
papayawhip = Key "papayawhip"

export
peachpuff : SVGColor
peachpuff = Key "peachpuff"

export
peru : SVGColor
peru = Key "peru"

export
pink : SVGColor
pink = Key "pink"

export
plum : SVGColor
plum = Key "plum"

export
powderblue : SVGColor
powderblue = Key "powderblue"

export
purple : SVGColor
purple = Key "purple"

export
red : SVGColor
red = Key "red"

export
rosybrown : SVGColor
rosybrown = Key "rosybrown"

export
royalblue : SVGColor
royalblue = Key "royalblue"

export
saddlebrown : SVGColor
saddlebrown = Key "saddlebrown"

export
salmon : SVGColor
salmon = Key "salmon"

export
sandybrown : SVGColor
sandybrown = Key "sandybrown"

export
seagreen : SVGColor
seagreen = Key "seagreen"

export
seashell : SVGColor
seashell = Key "seashell"

export
sienna : SVGColor
sienna = Key "sienna"

export
silver : SVGColor
silver = Key "silver"

export
skyblue : SVGColor
skyblue = Key "skyblue"

export
slateblue : SVGColor
slateblue = Key "slateblue"

export
slategray : SVGColor
slategray = Key "slategray"

export
slategrey : SVGColor
slategrey = Key "slategrey"

export
snow : SVGColor
snow = Key "snow"

export
springgreen : SVGColor
springgreen = Key "springgreen"

export
steelblue : SVGColor
steelblue = Key "steelblue"

export
tan : SVGColor
tan = Key "tan"

export
teal : SVGColor
teal = Key "teal"

export
thistle : SVGColor
thistle = Key "thistle"

export
tomato : SVGColor
tomato = Key "tomato"

export
transparent : SVGColor
transparent = Key "transparent"

export
turquoise : SVGColor
turquoise = Key "turquoise"

export
violet : SVGColor
violet = Key "violet"

export
wheat : SVGColor
wheat = Key "wheat"

export
white : SVGColor
white = Key "white"

export
whitesmoke : SVGColor
whitesmoke = Key "whitesmoke"

export
yellow : SVGColor
yellow = Key "yellow"

export
yellowgreen : SVGColor
yellowgreen = Key "yellowgreen"
