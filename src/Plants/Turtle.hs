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
import Data.Maybe (catMaybes, fromMaybe)
import Linear (V3(..), (!*!), (^*), (^/), cross, distance, norm)
import Numeric (showFFloat)

type Point = V3 Double

data Instruction
  = MovePenDown Point
  | MovePenUp Point
  | ChangeColor Int
  | StrokeWidth Double
  | Fill (Maybe Int)

instance Show Instruction where
  show (MovePenDown v) = "MovePenDown " <> showV3 v ""
  show (MovePenUp v) = "MovePenUp " <> showV3 v ""
  show (ChangeColor x) = "ChangeColor " <> show x
  show (StrokeWidth x) = "StrokeWidth " <> showFFloat (Just 4) x ""
  show (Fill x) = "Fill " <> show x

approxEq a b = approxZero $ b - a

approxZero a = abs a < epsilon

approxEqV3 a b = foldl (&&) True . fmap approxZero $ (b - a)

epsilon = 10.0 ** (-10)

instance Eq Instruction where
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
        rotateU (pi / 2.0) !*!
        V3 (V3 1.0 0.0 0.0) (V3 0.0 1.0 0.0) (V3 0.0 0.0 1.0)
    , _turtlePosition = V3 0 0 0
    , _turtleColor = 0
    , _turtleStrokeWidth = 1
    , _turtleFill = Nothing
    }

type TurtleStack = (TurtleState, [TurtleState])

type TurtleM = RWS Double () TurtleStack [Instruction]

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
letterToInstruction '&' a = rotateTurtle rotateL a
letterToInstruction '^' a = rotateTurtle (rotateL . negate) a
letterToInstruction '\\' a = rotateTurtle rotateH a
letterToInstruction '/' a = rotateTurtle (rotateH . negate) a
letterToInstruction '$' a =
  modifying (peek . turtleOrientation) orientToHorizon >> return mempty
letterToInstruction '|' _ = local (const 180) $ rotateTurtle rotateU Nothing
letterToInstruction '\'' Nothing = changeColor (+ 1)
letterToInstruction '\'' (Just a) = changeColor (const . round $ a)
letterToInstruction '!' Nothing = changeStroke 1.0
letterToInstruction '!' (Just a) = changeStroke a
letterToInstruction '{' a = changeFill a
letterToInstruction '}' _ = changeFill Nothing
letterToInstruction x a = return mempty

centreAndScale :: Double -> [Instruction] -> [Instruction]
centreAndScale size is =
  let isWithOrigin = MovePenUp (V3 0 0 0):is
      allPoints = catMaybes . map instructionToPoint $ isWithOrigin
      xs = map (\(V3 x _ _) -> x) allPoints
      ys = map (\(V3 _ y _) -> y) allPoints
      zs = map (\(V3 _ _ z) -> z) allPoints
      ps = V3 xs ys zs
      (mn, mx) =
        if any null ps
          then (V3 0 0 0, V3 0 0 0)
          else (fmap minimum ps, fmap maximum ps)
      midpoint =
        if mx == mn
          then (negate mx)
          else (mx - mn) ^/ (-2) - mn
      longestBoundingAxis = maximum (mx - mn)
   in map
        (scale
           (if longestBoundingAxis == 0
              then 1
              else size / longestBoundingAxis) .
         transform midpoint)
        isWithOrigin
  where
    instructionToPoint (MovePenDown x) = Just x
    instructionToPoint (MovePenUp x) = Just x
    instructionToPoint _ = Nothing
    transform dv (MovePenDown x) = MovePenDown $ x + dv
    transform dv (MovePenUp x) = MovePenUp $ x + dv
    transform _ x = x
    scale dv (MovePenDown x) = MovePenDown $ x ^* dv
    scale dv (MovePenUp x) = MovePenUp $ x ^* dv
    scale _ x = x

-- Algorithm described on pp57
orientToHorizon (V3 h l u) =
  let v = V3 0.0 1.0 0.0
      vxh = cross v h
      l' =
        case norm vxh of
          0 -> l
          d -> vxh ^/ d
      u' = cross h l'
   in (V3 h l' u')

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
    (\x -> r (toRadians $ theta * fromMaybe 1 a) !*! x)
  return mempty

changeFill a = do
  let fillColor = round <$> a
  assign (peek . turtleFill) fillColor
  return [Fill fillColor]

changeColor f = do
  modifying (peek . turtleColor) f
  color <- use $ peek . turtleColor
  return [ChangeColor color]

changeStroke a = do
  assign (peek . turtleStrokeWidth) a
  return [StrokeWidth a]

traceState :: TurtleM -> TurtleM
traceState m = do
  r <- m
  p <- use $ peek . turtlePosition
  o <- use $ peek . turtleOrientation
  traceM ""
  traceM $ "p: " <> show p
  traceM $ "o: " <> showOrientation o
  return r

moduleToInstruction :: LSystem -> ModuleFixed -> TurtleM
moduleToInstruction system m =
  let firstLetter = head $ view moduleSymbol m
      firstParam = headMaybe $ view moduleParams m
   in letterToInstruction firstLetter firstParam

interpret :: LSystem -> [Instruction]
interpret = interpretWith initialTurtle

interpretWith :: TurtleState -> LSystem -> [Instruction]
interpretWith state system =
  let MWord axiom = view lsysAxiom system
      theta = view lsysTheta system
      is =
        concat $
        fst $
        evalRWS (mapM (moduleToInstruction system) axiom) theta (state, [])
   in is

rotateU a = V3 (V3 (cos a) (sin a) 0) (V3 ((-1) * sin a) (cos a) 0) (V3 0 0 1)

rotateL a = V3 (V3 (cos a) 0 (sin a * (-1))) (V3 0 1 0) (V3 (sin a) 0 (cos a))

rotateH a = V3 (V3 1 0 0) (V3 0 (cos a) (sin a * (-1))) (V3 0 (sin a) (cos a))

askTheta = ask
