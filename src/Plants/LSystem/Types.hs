{-# LANGUAGE TemplateHaskell #-}

module Plants.LSystem.Types where

import Control.Lens (makeLenses)
import qualified Data.HashMap.Strict as M
import Data.Foldable (toList)
import Data.List (intercalate)
import Data.String (IsString(..))
import qualified Data.Sequence as S
import Linear (V3(..), V2(..))
import Numeric (showFFloat)

showV3 v =
  let (V3 x y z) = fmap (showFFloat (Just 2)) v
   in showString "<" . x . showString " " . y . showString " " . z .
      showString ">"

showV2 v =
  let (V2 x y) = fmap (showFFloat (Just 2)) v
   in showString "<" . x . showString " " . y . showString ">"

showOrientation (V3 h u l) =
  showString "H: " . showV3 h . showString " U: " <> showV3 u <>
  showString " L: " <>
  showV3 l $
  ""

-- A module is the fundamental unit of an LSystem. The parameter type is
-- variable because modules are used in different contexts whether as part of
-- the definition of the system or when it is being executed.
data Module a = Module
  { _moduleSymbol :: String
  , _moduleParams :: [a]
  } deriving (Eq)

instance Show a => Show (Module a) where
  show (Module {_moduleSymbol = s, _moduleParams = ps}) =
    s <>
    if null ps
      then ""
      else show ps

makeLenses ''Module

-- A fixed module contains actual values for its parameters, and is the
-- building block for the axiom of a system.
type ModuleFixed = Module Double

-- A module pattern is used when defining match rules, where each parameter can
-- be represented by a variable name.
type ModulePattern = Module String

-- A module pattern is used to define a match replacement. Rather than just
-- variable names, it can contain complete expressions.
type ModuleExpr = Module Expr

-- Expr is a simple mathematical expression involving constants, variables, and
-- operators.
data Expr
  = ExprConst Double
  | ExprVar String
  | ExprOp2 Term2
            Expr
            Expr
  deriving (Show, Eq)

data Term2
  = Sum
  | Product
  | Fraction
  | Exponent
  deriving (Show, Eq)

data Tree a = Root [Tree a] | Node a [Tree a] deriving (Show)

-- MWord is a newtype wrapper around a list of modules
data MWord a =
  MWord (S.Seq a)
  deriving (Eq)

mwordFromList = MWord . S.fromList
unwrapMWord (MWord a) = a

instance Semigroup (MWord x) where
  (MWord a) <> (MWord b) = MWord $ a <> b

instance Monoid (MWord a) where
  mempty = MWord mempty

instance Show a => Show (MWord a) where
  show (MWord l) = intercalate " " . map show . toList $ l

data Env =
  Env (M.HashMap String Expr)
  deriving (Show)

instance Semigroup Env where
  Env a <> Env b = Env (a <> b)

instance Monoid Env where
  mempty = Env mempty

data MatchRule = MatchRule
  { _ruleLetter :: ModulePattern
  , _ruleLetterPre :: Maybe ModulePattern
  , _ruleLetterPost :: Maybe ModulePattern
  , _ruleGuard :: MatchGuard
  } deriving (Show)

makeRule pattern =
  MatchRule
    { _ruleLetter = pattern
    , _ruleLetterPre = Nothing
    , _ruleLetterPost = Nothing
    , _ruleGuard = MatchAll
    }

data MatchGuard
  = MatchAll
  | MatchGuard String
               Expr
               Expr
  deriving (Show, Eq)

makeLenses ''MatchRule

data Production = Production
  { _prodRule :: MatchRule
  , _prodReplacement :: MWord ModuleExpr
  } deriving (Show)

makeLenses ''Production

data LSystem = LSystem
  { _lsysAxiom :: MWord ModuleFixed
  , _lsysN :: Int
  , _lsysTheta :: Double
  , _lsysProductions :: [Production]
  , _lsysIgnore :: [ModuleFixed]
  , _lsysDefines :: Env
  , _lsysSeed :: Int
  } deriving (Show)

makeLenses ''LSystem
