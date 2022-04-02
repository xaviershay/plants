{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

module Plants.Turtle where

import Plants.LSystem
import Plants.LSystem.Types
import Plants.Prelude

import Control.Lens
  ( ASetter
  , Lens'
  , _1
  , _2
  , assign
  , makeLenses
  , modifying
  , over
  , set
  , use
  , view
  )
import Control.Monad.RWS hiding (Product, Sum)
import Control.Monad.State (State(..), evalState, get, gets, modify, runState)
import Data.Maybe (fromMaybe)
import Linear.Matrix ((!*!))
import Linear.V3 (V3(..))
import Linear.Vector ((^*))

type Point = V3 Double

data Instruction a
  = MovePenDown a
  | MovePenUp a
  | ChangeColor Int
  | StrokeWidth Double
  | Fill Int
  deriving (Show)

approxEq a b = approxZero $ b - a

approxZero a = abs a < epsilon

approxEqV3 a b = foldl (&&) True . fmap approxZero $ (b - a)

epsilon = 10.0 ** (-10)

instance Eq (Instruction Point) where
  MovePenUp a == MovePenUp b = approxEqV3 a b
  MovePenDown a == MovePenDown b = approxEqV3 a b
  ChangeColor x == ChangeColor y = x == y
  StrokeWidth a == StrokeWidth b = approxEq a b
  Fill x == Fill y = x == y
  a == b = False

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
    { _turtleOrientation =
        (rotateU $ pi / 2.0) !*!
        V3 (V3 1.0 0.0 0.0) (V3 0.0 1.0 0.0) (V3 0.0 0.0 1.0)
    , _turtlePosition = V3 0 0 0
    , _turtleColor = 0
    , _turtleStrokeWidth = 1
    , _turtleFill = Nothing
    }

type TurtleStack = (TurtleState, [TurtleState])

type TurtleM = RWS Double () TurtleStack [Instruction Point]

peek :: Lens' TurtleStack TurtleState
peek f parent = fmap (\x -> set _1 x parent) (f . fst $ parent)

push :: TurtleM
push = do
  current <- use peek
  modifying _2 ((:) current)
  return mempty

pop :: TurtleM
pop = do
  top <- head <$> use _2
  modifying _2 (drop 1)
  assign _1 top
  return mempty

turtleHeading :: Lens' TurtleState Point
turtleHeading f parent =
  fmap
    (\x -> over turtleOrientation (setHeading x) parent)
    (f . getHeading . view turtleOrientation $ parent)
  where
    setHeading h (V3 _ u l) = V3 h u l
    getHeading (V3 h _ _) = h

letterToInstruction :: Char -> Maybe Double -> TurtleM
letterToInstruction '[' a = push
letterToInstruction ']' a = do
  p <- use $ peek . turtlePosition
  pop
  p' <- use $ peek . turtlePosition
  return $
    if not (p `approxEqV3` p')
      then [MovePenUp p']
      else []
letterToInstruction 'F' a = moveTurtle a MovePenDown
letterToInstruction 'f' a = moveTurtle a MovePenUp
letterToInstruction '+' a = rotateTurtle rotateU a
letterToInstruction '-' a = rotateTurtle (rotateU . negate) a
letterToInstruction x a = return mempty

moveTurtle a i = do
  h <- use $ peek . turtleHeading
  p <- use $ peek . turtlePosition
  let p' = p + h ^* fromMaybe 1 a
  assign (peek . turtlePosition) p'
  return [i p']

rotateTurtle r a = do
  theta <- askTheta
  modifying
    (peek . turtleOrientation)
    (\m -> m !*! r (toRadians $ theta * fromMaybe 1 a))
  return mempty

traceState :: TurtleM -> TurtleM
traceState m = do
  r <- m
  o <- use $ peek . turtleOrientation
  traceM $ "o: " <> show o
  return r

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
      theta = view lsysTheta system
   in concat $
      fst $ evalRWS (mapM (moduleToInstruction system) axiom) theta (state, [])

rotateU a = V3 (V3 (cos a) (sin a) 0) (V3 ((-1) * (sin a)) (cos a) 0) (V3 0 0 1)

rotateL a = V3 (V3 (cos a) 0 (sin a * (-1))) (V3 0 1 0) (V3 (sin a) 0 (cos a))

rotateH a = V3 (V3 1 0 0) (V3 0 (cos a) (sin a * (-1))) (V3 0 (sin a) (cos a))

askTheta = ask
