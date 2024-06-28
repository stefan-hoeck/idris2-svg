module Main

import Text.SVG

%default total

ex1 : SVGNode
ex1 =
  svg
    [ xmlns_2000, width 300.u, height 200.u ]
    [ rect [width 100.perc, height 100.perc, fill red]
    , circle [cx 150.u, cy 100.u, r 80.u, fill green]
    , text [x 150.u, y 125.u, fontSize 60.u, textAnchor "middle", fill white] "SVG"
    ]

main : IO ()
main = putStrLn (render ex1)
