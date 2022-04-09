{-# LANGUAGE OverloadedStrings #-}

module Turtle where

import TestPrelude

import Plants.Turtle

runTurtle word =
  interpret (set lsysAxiom word . set lsysTheta 90 $ emptyLSystem)

testTurtle = testTurtleWith id

testTurtleWith f word expected =
  testCase (show word) $
  expected @=?
  interpretWith
    (f (set
          turtleOrientation
          (V3 (V3 1 0 0) (V3 0 1 0) (V3 0 0 1))
          initialTurtle))
    (set lsysAxiom word . set lsysTheta 90 $ emptyLSystem)

test_centreAndScale =
  testGroup
    "centreAndScale"
    [ testCase "Scales to bounding box" $
      [MovePenUp (V3 (-50) (-50) (-50)), MovePenDown (V3 50 50 50)] @=?
      centreAndScale 100 [MovePenDown (V3 1 1 1)]
    , testCase "Scales based on param" $
      [MovePenUp (V3 (-25) (-25) (-25)), MovePenDown (V3 25 25 25)] @=?
      centreAndScale 50 [MovePenDown (V3 1 1 1)]
    , testCase "Scale adjusts to original size" $
      [MovePenUp (V3 (-25) (-25) (-25)), MovePenDown (V3 25 25 25)] @=?
      centreAndScale 50 [MovePenDown (V3 10 10 10)]
    , testCase "Scales to longest axis" $
      [MovePenUp (V3 (-25) (-25) (-15)), MovePenDown (V3 25 25 15)] @=?
      centreAndScale 50 [MovePenDown (V3 10 10 6)]
    , testCase "Zero instructions" $ [MovePenUp $ V3 0 0 0] @=? centreAndScale 50 []
    , testCase "Translates start position" $
      [MovePenUp $ V3 (-25) (-25) (-25), MovePenDown (V3 0 0 0), MovePenDown (V3 25 25 25)] @=?
      centreAndScale 50 [MovePenDown $ V3 1 1 1, MovePenDown $ V3 2 2 2]
    ]

test_interpret =
  testGroup
    "Turtle.interpret"
    [ testTurtle "F" [MovePenDown (V3 1 0 0)]
    , testTurtle "F(2)" [MovePenDown (V3 2 0 0)]
    , testTurtleWith (set turtleHeading (V3 0 0 1)) "F" [MovePenDown (V3 0 0 1)]
    , testTurtle "F F" [MovePenDown (V3 1 0 0), MovePenDown (V3 2 0 0)]
    , testTurtle "f" [MovePenUp (V3 1 0 0)]
    , testTurtle "f(2)" [MovePenUp (V3 2 0 0)]
    , testTurtleWith (set turtleHeading (V3 0 0 1)) "f" [MovePenUp (V3 0 0 1)]
    , testTurtle "f f" [MovePenUp (V3 1 0 0), MovePenUp (V3 2 0 0)]
    , testTurtle "+ F" [MovePenDown (V3 0 1 0)]
    , testTurtle "+(3) F" [MovePenDown (V3 0 (-1) 0)]
    , testTurtle "+ F + F" [MovePenDown (V3 0 1 0), MovePenDown (V3 (-1) 1 0)]
    , testTurtle "- F" [MovePenDown (V3 0 (-1) 0)]
    , testTurtle "-(3) F" [MovePenDown (V3 0 1 0)]
    , testTurtle
        "- F - F"
        [MovePenDown (V3 0 (-1) 0), MovePenDown (V3 (-1) (-1) 0)]
    , testTurtle "[ + ] F" [MovePenDown (V3 1 0 0)]
    , testTurtle
        "[ + [ - ] F ] F"
        [MovePenDown (V3 0 1 0), MovePenUp (V3 0 0 0), MovePenDown (V3 1 0 0)]
    , testTurtle
        "F [ F ] F"
        [ MovePenDown (V3 1 0 0)
        , MovePenDown (V3 2 0 0)
        , MovePenUp (V3 1 0 0)
        , MovePenDown (V3 2 0 0)
        ]
    , testTurtle "F | F" [MovePenDown (V3 1 0 0), MovePenDown (V3 0 0 0)]
    , testTurtle
        "{(1) F } F"
        [ Fill $ Just 1
        , MovePenDown (V3 1 0 0)
        , Fill Nothing
        , MovePenDown (V3 2 0 0)
        ]
    , testTurtle "& F" [MovePenDown (V3 0 0 (-1))]
    , testTurtle "&(3) F" [MovePenDown (V3 0 0 1)]
    , testTurtle
        "& F & F"
        [MovePenDown (V3 0 0 (-1)), MovePenDown (V3 (-1) 0 (-1))]
    , testTurtle "^ F" [MovePenDown (V3 0 0 1)]
    , testTurtle "^(3) F" [MovePenDown (V3 0 0 (-1))]
    , testTurtle "^ F ^ F" [MovePenDown (V3 0 0 1), MovePenDown (V3 (-1) 0 1)]
    , testTurtle "\\ F" [MovePenDown (V3 1 0 0)]
    , testTurtle "\\ & F" [MovePenDown (V3 0 (-1) 0)]
    , testTurtle "\\ F + F" [MovePenDown (V3 1 0 0), MovePenDown (V3 1 0 (-1))]
    , testTurtle "+ F \\ F" [MovePenDown (V3 0 1 0), MovePenDown (V3 0 2 0)]
    , testTurtle "/ F" [MovePenDown (V3 1 0 0)]
    , testTurtle "/ & F" [MovePenDown (V3 0 1 0)]
    , testTurtle "/ F + F" [MovePenDown (V3 1 0 0), MovePenDown (V3 1 0 1)]
    , testTurtle "+ F / F" [MovePenDown (V3 0 1 0), MovePenDown (V3 0 2 0)]
    , testTurtle "+ &(0.5) / $ &(0.5) F" [MovePenDown (V3 0 0 (-1))]
    , testTurtle "+ $ F" [MovePenDown (V3 0 1 0)]
    , testTurtle "'" [ChangeColor 1]
    , testTurtle "' '" [ChangeColor 1, ChangeColor 2]
    , testTurtle "'(2)" [ChangeColor 2]
    , testTurtle "!(0.8)" [StrokeWidth 0.8]
    , testTurtle "!(0.8) !" [StrokeWidth 0.8, StrokeWidth 1]
    ]
