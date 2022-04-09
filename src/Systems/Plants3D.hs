{-# LANGUAGE OverloadedStrings #-}

module Systems.Plants3D where

import Plants.Prelude
import Plants.LSystem

-- "Monopodial tree-like structures of Honda" p. 56
hondaSystem :: Double -> Double -> Double -> Double -> LSystem
hondaSystem r1 r2 a0 a2 =
  lsystem $ do
    n 10
    theta 1
    axiom "A(1, 10)"
    define
            [ ("r1", showFullPrecision r1) -- contraction ratio for the trunk
            , ("r2", showFullPrecision r2) -- contraction ratio for branches
            , ("a0", showFullPrecision a0) -- branching angle from the trunk
            , ("a2", showFullPrecision a2) -- branching angle for lateral axes
            , ("d", "137.5") -- divergence angle
            , ("wr", "0.707") -- width decrease rate
            ]
    productions
      [ ( "A(l, w)"
        , "!(w) F(l) [ &(a0) B(l*r2,w*wr)   ] /(d) A(l*r1,w*wr)")
      , ( "B(l, w)"
        , "!(w) F(l) [ -(a2) $ C(l*r2,w*wr) ]      C(l*r1, w*wr)")
      , ( "C(l, w)"
        , "!(w) F(l) [ +(a2) $ B(l*r2,w*wr) ]      B(l*r1, w*wr)")
      ]

honda 1 = hondaSystem 0.9 0.6 45 45
honda 2 = hondaSystem 0.9 0.9 45 45
honda 3 = hondaSystem 0.9 0.8 45 45
honda 4 = hondaSystem 0.9 0.7 30 (-30)
