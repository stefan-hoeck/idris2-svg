module Text.SVG.Types

import Derive.Prelude
import public Data.Refined

%default total
%language ElabReflection

public export
data SVGAngle : Type where
  Deg  : Double -> SVGAngle
  Rad  : Double -> SVGAngle
  Grad : Double -> SVGAngle

export
Interpolation SVGAngle where
  interpolate (Deg x)  = show x ++ "deg"
  interpolate (Rad x)  = show x ++ "rad"
  interpolate (Grad x) = show x ++ "grad"

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
--          SVGColor
--------------------------------------------------------------------------------

public export
data SVGColor : Type where
  RGB : (red,green,blue : Bits8) -> SVGColor
  Key : String -> SVGColor

export
Interpolation SVGColor where
  interpolate (RGB r g b) = "rgba(\{show r} \{show g} \{show b}})"
  interpolate (Key s)     = s

export
Show SVGColor where show = interpolate

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
  interpolate (MkPercentage v) = show v ++ "%"

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
--          Length
--------------------------------------------------------------------------------

public export
data Number : Type where
  I : Int32 -> Number
  D : Double -> Number

export %inline
FromDouble Number where fromDouble = D

export %inline
fromInteger : Integer -> Number
fromInteger = I . cast

export
Interpolation Number where
  interpolate (I i) = show i
  interpolate (D d) = show d

public export
data Length : Type where
  U        : Number -> Length
  Pt       : Number -> Length
  Px       : Number -> Length
  Mm       : Number -> Length
  Cm       : Number -> Length

export
Interpolation Length where
  interpolate (U x)  =  interpolate x
  interpolate (Pt x)  = interpolate x ++ "pt"
  interpolate (Px x)  = interpolate x ++ "px"
  interpolate (Mm x)  = interpolate x ++ "mm"
  interpolate (Cm x)  = interpolate x ++ "cm"

export %inline
u : Cast Length a => Number -> a
u = cast . U

export %inline
mm : Cast Length a => Number -> a
mm = cast . Mm

export %inline
cm : Cast Length a => Number -> a
cm = cast . Cm

export %inline
px : Cast Length a => Number -> a
px = cast . Px

export %inline
pt : Cast Length a => Number -> a
pt = cast . Pt

export %inline
(.u) : Cast Length a => Number -> a
(.u) = cast . U

export %inline
(.mm) : Cast Length a => Number -> a
(.mm) = cast . Mm

export %inline
(.cm) : Cast Length a => Number -> a
(.cm) = cast . Cm

export %inline
(.px) : Cast Length a => Number -> a
(.px) = cast . Px

export %inline
(.pt) : Cast Length a => Number -> a
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
  Move  : (rel : Bool) -> (x,y : Number) -> PathCmd
  Line  : (rel : Bool) -> (x,y : Number) -> PathCmd
  Horiz : (rel : Bool) -> (x : Number) -> PathCmd
  Vert  : (rel : Bool) -> (y : Number) -> PathCmd
  Z     : PathCmd
  Quadr : (rel : Bool) -> (x1,y1,x,y : Number) -> PathCmd
  QSucc : (rel : Bool) -> (x,y : Number) -> PathCmd
  Cubic : (rel : Bool) -> (x1,y1,x2,y2,x,y : Number) -> PathCmd
  CSucc : (rel : Bool) -> (x2,y2,x,y : Number) -> PathCmd
  Arc   :
       (rel : Bool)
    -> (rx,ry,rot : Number)
    -> (largeArc,sweep : Bool)
    -> (x,y : Number)
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
  interpolate (Line rel x y) = letter rel "M" "m" ++ "\{x} \{y}"
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
  M : (x,y : Number) -> PathCmd
  M = Move False

  export %inline
  m : (x,y : Number) -> PathCmd
  m = Move True

  export %inline
  L : (x,y : Number) -> PathCmd
  L = Line False

  export %inline
  l : (x,y : Number) -> PathCmd
  l = Line True

  export %inline
  H : (x : Number) -> PathCmd
  H = Horiz False

  export %inline
  h : (x : Number) -> PathCmd
  h = Horiz True

  export %inline
  V : (x : Number) -> PathCmd
  V = Vert False

  export %inline
  v : (x : Number) -> PathCmd
  v = Vert True

  export %inline
  S : (x2,y2,x,y : Number) -> PathCmd
  S = CSucc False

  export %inline
  s : (x2,y2,x,y : Number) -> PathCmd
  s = CSucc True

  export %inline
  C : (x1,y1,x2,y2,x,y : Number) -> PathCmd
  C = Cubic False

  export %inline
  c : (x1,y1,x2,y2,x,y : Number) -> PathCmd
  c = Cubic True

  export %inline
  T : (x,y : Number) -> PathCmd
  T = QSucc False

  export %inline
  t : (x,y : Number) -> PathCmd
  t = QSucc True

  export %inline
  Q : (x1,y1,x,y : Number) -> PathCmd
  Q = Quadr False

  export %inline
  q : (x1,y1,x,y : Number) -> PathCmd
  q = Quadr True

  export %inline
  A   :
       (rx,ry,rot : Number)
    -> (largeArc,sweep : Bool)
    -> (x,y : Number)
    -> PathCmd
  A = Arc False

  export %inline
  a   :
       (rx,ry,rot : Number)
    -> (largeArc,sweep : Bool)
    -> (x,y : Number)
    -> PathCmd
  a = Arc True

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
