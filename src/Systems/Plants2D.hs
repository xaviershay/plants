{-# LANGUAGE OverloadedStrings #-}

module Systems.Plants2D where

import Plants.LSystem

-- Branching structue examples from p. 25
branching 1 =
  lsystem $ do
    n 4
    theta 25.7
    axiom "F"
    productions [("F", "F [ + F ] F [ - F ] F")]
branching 2 =
  lsystem $ do
    n 4
    theta 20
    axiom "F"
    productions [("F", "F [ + F ] F [ - F ] [ F ]")]
branching 3 =
  lsystem $ do
    n 4
    theta 22.5
    axiom "F"
    productions [("F", "F F - [ - F + F + F ] + [ + F - F - F ]")]
branching 4 =
  lsystem $ do
    n 7
    theta 20
    axiom "X"
    productions [("X", "F [ + X ] F [ - X ] + X"), ("F", "F F")]
branching 5 =
  lsystem $ do
    n 6
    theta 25.7
    axiom "X"
    productions [("X", "F [ + X ] [ - X ] F X"), ("F", "F F")]
branching 6 =
  lsystem $ do
    n 5
    theta 22.5
    axiom "X"
    productions [("X", "F - [ [ X ] + X ] + F [ + F X ] - X"), ("F", "F F")]
