{-# LANGUAGE OverloadedStrings #-}

module Turtle where

import TestPrelude

import Plants.Turtle

runTurtle word = interpret (set lsysAxiom word . set lsysTheta 90 $ emptyLSystem)

testTurtle = testTurtleWith id

testTurtleWith f word expected =
  testCase (show word) $
  expected @=?
  interpretWith
    (f (set turtleOrientation (V3 (V3 1 0 0) (V3 0 1 0) (V3 0 0 1))
       initialTurtle))
    (set lsysAxiom word . set lsysTheta 90 $ emptyLSystem)

test_interpret =
  testGroup
    "Turtle.interpret"
    [ testTurtle "F" [MovePenDown (V3 1 0 0)]
    , testTurtle "F(2)" [MovePenDown (V3 2 0 0)]
    , testTurtleWith (set turtleHeading (V3 0 0 1)) "F" [MovePenDown (V3 0 0 1)]
    , testTurtle "f" [MovePenUp (V3 1 0 0)]
    , testTurtle "f(2)" [MovePenUp (V3 2 0 0)]
    , testTurtleWith (set turtleHeading (V3 0 0 1)) "f" [MovePenUp (V3 0 0 1)]
    , testTurtle "+ F" [MovePenDown (V3 0 1 0)]
    ]
