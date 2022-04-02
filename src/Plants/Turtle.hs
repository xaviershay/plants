{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}

module Plants.Turtle where

import Plants.LSystem
import Plants.LSystem.Types
import Plants.Prelude

import Control.Lens
  ( ASetter
  , Lens'
  , _1
  , assign
  , makeLenses
  , modifying
  , over
  , set
  , use
  , view
  )
import Control.Monad.State (State(..), evalState, get, gets, modify, runState)
import Data.Maybe (fromMaybe)
import Linear.V3 (V3(..))
import Linear.Vector ((^*))

type Point = V3 Double

data Instruction a
  = MovePenDown a
  | MovePenUp a
  | ChangeColor Int
  | StrokeWidth Double
  | Fill Int
  deriving (Show, Eq)

data TurtleState = TurtleState
  { _turtleOrientation :: V3 Point
  , _turtlePosition :: Point
  , _turtleColor :: Int
  , _turtleStrokeWidth :: Double
  , _turtleFill :: Maybe Int
  } deriving (Show)

makeLenses ''TurtleState

initialTurtle =
  TurtleState
  -- We're making trees here, most of the time we want them to grow up, so
  -- let's start by pointing directly along the Y-axis.
  --rotateM = (rotateU $ pi / 2.0) !*! V3 (V3 1.0 0.0 0.0) (V3 0.0 1.0 0.0) (V3 0.0 0.0 1.0),
    { _turtleOrientation = V3 (V3 1 0 0) (V3 0 1 0) (V3 0 0 1)
    , _turtlePosition = V3 0 0 0
    , _turtleColor = 0
    , _turtleStrokeWidth = 1
    , _turtleFill = Nothing
    }

type TurtleStack = (TurtleState, [TurtleState])

type TurtleM = State TurtleStack [Instruction Point]

peek :: Lens' TurtleStack TurtleState
peek f parent = fmap (\x -> set _1 x parent) (f . fst $ parent)

turtleHeading :: Lens' TurtleState Point
turtleHeading f parent =
  fmap
    (\x -> over turtleOrientation (setHeading x) parent)
    (f . getHeading . view turtleOrientation $ parent)
  where
    setHeading h (V3 _ u l) = V3 h u l
    getHeading (V3 h _ _) = h

letterToInstruction :: Char -> Maybe Double -> TurtleM
letterToInstruction 'F' a = moveTurtle a MovePenDown
letterToInstruction 'f' a = moveTurtle a MovePenUp
letterToInstruction x a = error $ "unknown instruction: " <> show x <> " / " <> show a

moveTurtle a i = do
  h <- use $ peek . turtleHeading
  p <- use $ peek . turtlePosition
  let h' = h ^* fromMaybe 1 a
  assign (peek . turtlePosition) h'
  return [i h']

moduleToInstruction :: LSystem -> ModuleFixed -> TurtleM
moduleToInstruction system m =
  let firstLetter = head $ view moduleSymbol m
      firstParam = headMaybe $ view moduleParams m
   in letterToInstruction firstLetter firstParam

interpret :: LSystem -> [Instruction Point]
interpret = interpretWith initialTurtle

interpretWith :: TurtleState -> LSystem -> [Instruction Point]
interpretWith state system =
  let MWord axiom = view lsysAxiom system
   in concat $
      evalState (mapM (moduleToInstruction system) axiom) (state, [])
