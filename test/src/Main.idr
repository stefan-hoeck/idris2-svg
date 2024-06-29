module Main

import Text.SVG.Attribute as A
import Text.SVG

%default total

ex1 : SVGNode
ex1 =
  svg
    [ xmlns_2000, width 300.u, height 200.u, viewBox 20.u 30.u 100.u 50.u ]
    [ rect [width 300.u, height 200.u, fill red]
    , circle [cx 150.u, cy 100.u, r 80.u, fill green]
    , text [x 150.u, y 125.u, fontSize 60.u, textAnchor Middle, fill white] "SVG"
    ]

grp2 : SVGColor -> List SVGNode -> SVGNode
grp2 c = g [stroke c, fill transparent, strokeWidth 5.u]

ex2 : SVGNode
ex2 =
  svg
    [ xmlns_2000, width 200.u, height 250.u ]
    [ grp2 black
        [ rect [x 10.u, y 10.u, width 30.u, height 30.u]
        , rect [x 60.u, y 10.u, rx 10.u, ry 10.u, width 30.u, height 30.u]
        ]
    , grp2 red
        [ circle [cx 25.u, cy 75.u, r 20.u]
        , ellipse [cx 75.u, cy 75.u, rx 20.u, ry 5.u]
        ]
    , grp2 orange
        [ line [x1 10.u, x2 50.u, y1 110.u, y2 150.u]
        , polyline [points [60,110,65,120,70,115,75,130,80,125,85,140,90,135,95,150,100,145]]
        ]
    , polygon
        [ points [50,160,55,180,70,180,60,190,65,205,50,195,35,205,40,190,30,180,45,180]
        , stroke green
        , fill transparent
        , strokeWidth 5.u
        ]
    , path
        [ d [M 20 230, Q 40 205 50 230, T 90 230]
        , fill none
        , stroke blue
        , strokeWidth 5.u
        ]
    ]

visualizeCubic : (a,b,x1,y1,x2,y2,x,y : Number) -> List SVGNode
visualizeCubic a b x1 y1 x2 y2 x y =
  [ path
      [ d [ M a b, C x1 y1 x2 y2 x y], fill none, stroke black, strokeWidth 5.u ]
  , g [stroke red, strokeWidth 2.u]
      [ line   [ A.x1 a.u, A.y1 b.u, A.x2 x1.u, A.y2 y1.u]
      , line   [ A.x1 x2.u, A.y1 y2.u, A.x2 x.u, A.y2 y.u]
      ]
  , g [fill red, stroke none]
      [ circle [ A.cx a.u, A.cy b.u, A.r 5.u]
      , circle [ A.cx x1.u, A.cy y1.u, A.r 5.u]
      , circle [ A.cx x2.u, A.cy y2.u, A.r 5.u]
      , circle [ A.cx x.u, A.cy y.u, A.r 5.u]
      ]
  ]

ex3 : SVGNode
ex3 =
  svg
    [ xmlns_2000, width 1000.u, height 1000.u ] $
    visualizeCubic 10 10 50 100 150 100 190 10 ++
    visualizeCubic 10 200 50 300 190 300 190 200 ++
    visualizeCubic 10 400 50 500 250 500 190 400

main : IO ()
main = putStrLn (render ex3)
