{-# LANGUAGE OverloadedStrings #-}

module Systems.Geometric where

import Plants.LSystem

kochTiles =
  lsystem $ do
    axiom "F - F - F - F"
    theta 90
    n 3
    productions [("F", "F F - F + F - F - F F")]

kochSpiral =
  lsystem $ do
    axiom "F - F - F - F"
    theta 90
    n 4
    productions [("F", "F - F + F - F - F")]
