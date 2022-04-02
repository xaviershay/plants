{-# LANGUAGE OverloadedStrings #-}

module Turtle where

import TestPrelude

import Plants.Turtle

runTurtle word = interpret (set lsysAxiom word emptyLSystem)

testTurtle = testTurtleWith id

testTurtleWith f word expected =
  testCase (show word) $
  expected @=? interpretWith (f initialTurtle) (set lsysAxiom word emptyLSystem)

test_interpret =
  testGroup
    "Turtle.interpret"
    [ testTurtle "F" [MovePenDown (V3 1 0 0)]
    , testTurtle "F(2)" [MovePenDown (V3 2 0 0)]
    , testTurtleWith (set turtleHeading (V3 0 0 1)) "F" [MovePenDown (V3 0 0 1)]
    , testTurtle "f" [MovePenUp (V3 1 0 0)]
    , testTurtle "f(2)" [MovePenUp (V3 2 0 0)]
    , testTurtleWith (set turtleHeading (V3 0 0 1)) "f" [MovePenUp (V3 0 0 1)]
    ]
