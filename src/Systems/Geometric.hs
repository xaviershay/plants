{-# LANGUAGE OverloadedStrings #-}

module Systems.Geometric where

import Plants.LSystem

cube =
  lsystem $ do
    axiom "[ '(2) F(0.3) ] [ & F(0.3) ] [ + F(0.3) ] | f(0.5) + f(0.5) + ^ f(0.5) & [ ' S ] '(0) [ & s ] [ \\ s ] & f + f | / S ^ s"
    n 1
    theta 90
    productions [("S", "F + F + F + F +")]

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

kochIsland =
  lsystem $ do
    n 3
    theta 90.0
    axiom "F - F - F - F"
    productions [("F", "F - F + F + F F - F - F + F")]

islandLakes =
  lsystem $ do
    axiom "F + F + F + F"
    theta 90
    n 2
    productions
      [ ( "F"
        , "F + f - F F + F + F F + F f + F F - f + F F - F - F F - F f - F F F")
      ]

dragonCurve =
  lsystem $ do
    axiom "F◀"
    n 9
    theta 90.0
    productions [("F◀", "F◀ + F▶ +"), ("F▶", "- F◀ - F▶")]

gosperHexCurve =
  lsystem $ do
    axiom "FL"
    n 4
    theta 60.0
    productions
      [ ("FL", "FL + FR + + FR - FL - - FL FL - FR +")
      , ("FR", "- FL + FR FR + + FR + FL - - FL - FR")
      ]
