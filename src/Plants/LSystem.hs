{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}

module Plants.LSystem (LSystem, lsystem, axiom, productions, theta, n) where

import Plants.LSystem.Parser
import Plants.LSystem.Types
import Plants.LSystem.Eval

import Control.Lens (makeLenses, assign, set, view)
import Control.Monad.State
import Data.String (IsString(..))

instance IsString MatchRule where
  fromString x = makeRule (parsePatternUnsafe x)

instance IsString (MWord ModuleExpr) where
  fromString x = parseWordExprUnsafe x

instance IsString (MWord ModuleFixed) where
  fromString x = parseWordUnsafe x

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

