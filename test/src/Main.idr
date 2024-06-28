module Main

import Text.SVG

%default total

ex1 : SVGNode
ex1 =
  svg
    [ xmlns_2000, width 300.u, height 200.u, viewBox 20.u 30.u 100.u 50.u ]
    [ rect [width 300.u, height 200.u, fill red]
    , circle [cx 150.u, cy 100.u, r 80.u, fill green]
    , text [x 150.u, y 125.u, fontSize 60.u, textAnchor "middle", fill white] "SVG"
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
    ]
--
--   <path d="M20,230 Q40,205 50,230 T90,230" fill="none" stroke="blue" stroke-width="5"/>

main : IO ()
main = putStrLn (render ex2)
