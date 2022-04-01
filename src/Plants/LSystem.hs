{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}

module Plants.LSystem (lsystem, axiom, productions, theta, n) where

import Plants.LSystem.Parser
import Plants.LSystem.Types

import Control.Lens (makeLenses, assign, set)
import Control.Monad.State
import Data.String (IsString(..))
import qualified Data.HashMap.Strict as M



data Env = Env (M.HashMap String Expr)
  deriving (Show)

instance Semigroup Env where
  Env a <> Env b = Env (a <> b)

instance Monoid Env where
  mempty = Env mempty

data MatchRule = MatchRule {
  _ruleLetter :: ModulePattern,
  _ruleLetterPre :: Maybe ModulePattern,
  _ruleLetterPost :: Maybe ModulePattern,
  _ruleGuard :: MatchGuard
} deriving (Show)

makeRule pattern = MatchRule {
  _ruleLetter = pattern,
  _ruleLetterPre = Nothing,
  _ruleLetterPost = Nothing,
  _ruleGuard = MatchAll
}

data MatchGuard = MatchAll | MatchGuard String Expr Expr
  deriving (Show, Eq)

makeLenses ''MatchRule

instance IsString MatchRule where
  fromString x = makeRule (parsePatternUnsafe x)

instance IsString (MWord ModuleExpr) where
  fromString x = parseWordExprUnsafe x

instance IsString (MWord ModuleFixed) where
  fromString x = parseWordUnsafe x

data Production = Production {
  _prodRule :: MatchRule,
  _prodReplacement :: MWord ModuleExpr
} deriving (Show)

makeLenses ''Production

data LSystem = LSystem {
  _lsysAxiom :: MWord ModuleFixed,
  _lsysN :: Int,
  _lsysTheta :: Double,
  _lsysProductions :: [Production],
  _lsysIgnore :: [ModuleFixed],
  _lsysDefines :: Env,
  _lsysSeed :: Int
}

makeLenses ''LSystem

type LSystemBuilder = State LSystem ()

emptyLSystem = LSystem {
  _lsysAxiom = mempty,
  _lsysN = 0,
  _lsysTheta = 0,
  _lsysProductions = mempty,
  _lsysIgnore = mempty,
  _lsysDefines = mempty,
  _lsysSeed = 0
}

axiom :: MWord ModuleFixed -> LSystemBuilder
axiom = assign lsysAxiom

n :: Int -> LSystemBuilder
n = assign lsysN

theta :: Double -> LSystemBuilder
theta = assign lsysTheta

productions :: [(MatchRule, MWord ModuleExpr)] -> LSystemBuilder
productions ps = do
  let x = map (\(a, b) -> Production {
    _prodRule = a,
    _prodReplacement = b
  }) ps
  assign lsysProductions x

lsystem :: LSystemBuilder -> LSystem
lsystem m = snd $ runState m emptyLSystem
