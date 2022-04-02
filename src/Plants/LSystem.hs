{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}

module Plants.LSystem (module Plants.LSystem, module Plants.LSystem.Types, module Plants.LSystem.Eval) where

import Plants.LSystem.Parser
import Plants.LSystem.Types
import Plants.LSystem.Eval

import Control.Lens (makeLenses, assign, set, view)
import Control.Monad.State
import Data.String (IsString(..))
import qualified Data.HashMap.Strict as M

instance IsString MatchRule where
  fromString x = makeRule (parsePatternUnsafe x)

instance IsString (MWord ModuleExpr) where
  fromString x = parseWordExprUnsafe x

instance IsString (MWord ModuleFixed) where
  fromString x = parseWordUnsafe x

instance IsString ModulePattern where
  fromString x = parsePatternUnsafe x

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

ignore :: MWord ModuleFixed -> LSystemBuilder
ignore (MWord x) = assign lsysIgnore x

define :: [(String, String)] -> LSystemBuilder
define = assign lsysDefines . Env . M.map parseExprUnsafe . M.fromList

productions :: [(MatchRule, MWord ModuleExpr)] -> LSystemBuilder
productions ps = do
  let x = map (\(a, b) -> Production {
    _prodRule = a,
    _prodReplacement = b
  }) ps
  assign lsysProductions x

lsystem :: LSystemBuilder -> LSystem
lsystem m = snd $ runState m emptyLSystem

(<|) :: ModulePattern -> MatchRule -> MatchRule
l <| r = set ruleLetterPre (Just l) r
(|>) :: MatchRule -> ModulePattern -> MatchRule
r |> l = set ruleLetterPost (Just l) r

-- (|:) :: MatchRule -> String -> MatchRule
-- rule |: guardString = set ruleGuard (parseGuardUnsafe guardString) rule
