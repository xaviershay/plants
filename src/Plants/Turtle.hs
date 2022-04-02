module Plants.Turtle where

import Plants.LSystem

import Linear.V3 (V3)

type Point = V3 Double

data Instruction a =
    MovePenDown a
  | MovePenUp a
  | ChangeColor Int
  | StrokeWidth Double
  | Fill Int
  deriving (Show)

interpret :: LSystem -> [Instruction Point]
interpret = undefined
