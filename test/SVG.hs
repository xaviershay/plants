{-# LANGUAGE OverloadedStrings #-}

module SVG where

import Linear
import TestPrelude

import Plants.SVG

test_SVG =
  testGroup
    "bounds"
    [ testCase "max/min same for single point" $
      (V2 (-1) (-1), V2 (-1) (-1)) @=? bounds [mkSVGPath (V2 (-1) (-1))]
    , testCase "max/min for two points" $
      (V2 1 2, V2 3 4) @=? bounds [mkSVGPath (V2 1 4), mkSVGPath (V2 3 2)]
    , testCase "zeroes when no points" $
      (V2 0 0, V2 0 0) @=? bounds []
    ]
