{-# LANGUAGE OverloadedStrings #-}

module Plants.LSystem.Eval where

import Plants.LSystem.Types
import Plants.Prelude

import Control.Lens (assign, makeLenses, over, set, view)
import qualified Data.HashMap.Strict as M
import Data.List (partition, tails)
import Data.Maybe (listToMaybe)
import System.Random.Stateful (StatefulGen, mkStdGen, runStateGen, uniformRM)

type ModuleContext = (ModuleFixed, Maybe ModuleFixed, [ModuleFixed])

data TreeBuilder a
  = Child a
  | Sibling (Tree a)
  deriving (Show)

gatherContext ignores m parents children =
  ( m
  , listToMaybe . take 1 . filter (not . ignored) $ parents
  , immediates children)
  where
    immediates = concatMap f2
    f2 (Root xs) = immediates xs
    f2 (Node x cs) =
      if ignored x
        then immediates cs
        else [x]
    ignored x =
      view moduleSymbol x `elem` ("[" : "]" : map (view moduleSymbol) ignores)

type TreeTraverser a b = a -> [a] -> [Tree a] -> b

mapTree :: TreeTraverser a b -> Tree a -> [b]
mapTree f t = mapTree' mempty t
  where
    mapTree' parent (Root cs) = concatMap (mapTree' parent) . reverse $ cs
    mapTree' parent (Node x cs) =
      (f x parent $ reverse cs) : mapTree' (x : parent) (Root cs)

buildTreeWith :: (a -> String) -> [a] -> Tree a
buildTreeWith mapper = fst . blah'
  where
    f ns (Child x:rest) = (Node x (f [] rest) : ns)
    f ns (Sibling x:rest) = f (x : ns) rest
    f tree [] = tree
    blah' ms =
      let (treeInstructions, remainder) = extractTree ms
       in (Root $ f [] treeInstructions, remainder)
    extractTree [] = ([], [])
    extractTree (m:remainder) =
      case mapper m of
        "[" ->
          let (ts, rest) = blah' remainder
           in let (x, y) = extractTree rest
               in ([Sibling (Node m [ts])] <> x, y)
        "]" -> ([Child m], remainder)
        x ->
          let (ts, rest) = extractTree remainder
           in ([Child m] <> ts, rest)

axiomWithContext ignores =
  mapTree (gatherContext ignores) . buildTreeWith (view moduleSymbol)

step :: StatefulGen g m => LSystem -> g -> m LSystem
step system gen = do
  let MWord axiom = view lsysAxiom system
  let ignores = view lsysIgnore system
  let axiomContext = axiomWithContext ignores axiom
  parts <- mapM (findProduction system gen) axiomContext
  return $
    set lsysAxiom (foldl (<>) mempty . map replacementWithContext $ parts) .
    over lsysN (\x -> x - 1) $
    system

runM :: StatefulGen g m => LSystem -> g -> m LSystem
runM system gen =
  case (view lsysN system) of
    0 -> return system
    i -> do
      blah <- step system gen
      runM blah gen

run :: LSystem -> LSystem
run system = fst $ runStateGen (mkStdGen . view lsysSeed $ system) (runM system)

replacementWithContext :: (Production, Env) -> MWord ModuleFixed
replacementWithContext (p, env) =
  let MWord replacementWord = view prodReplacement p
   in MWord . map f $ replacementWord
  where
    f l =
      Module
        { _moduleSymbol = view moduleSymbol l
        , _moduleParams = map (eval env) (view moduleParams l)
        }

eval :: Env -> Expr -> Double
eval env expr =
  let y = eval' env expr
   in y

eval' _ (ExprConst d) = d
eval' env@(Env varMap) (ExprVar d) =
  eval env (M.findWithDefault (ExprConst 0) d varMap)
eval' env (ExprOp2 Sum a b) = eval env a + eval env b
eval' env (ExprOp2 Product a b) = eval env a * eval env b
eval' env (ExprOp2 Fraction a b) = eval env a / eval env b
eval' env (ExprOp2 Exponent a b) = eval env a ** eval env b

envFromContext :: Production -> ModuleContext -> Env
envFromContext p (l1, l2, l3) =
  Env . M.fromList $
  envForLetter (Just $ view ruleLetter rule) (Just l1) <>
  envForLetter (view ruleLetterPre rule) l2 <>
  envForLetter (view ruleLetterPost rule) (headMaybe l3) -- TODO
  where
    rule = view prodRule p
    envForLetter :: Maybe ModulePattern -> Maybe ModuleFixed -> [(String, Expr)]
    envForLetter pat l =
      case (pat, l) of
        (Just p', Just l') ->
          let paramLabels = view moduleParams p'
              paramValues = map ExprConst . (view moduleParams) $ l'
           in zip paramLabels paramValues
        _ -> []

emptyEnv = Env mempty

hasGuard :: Production -> Bool
hasGuard = (/= MatchAll) . view (prodRule . ruleGuard)

findProduction ::
     StatefulGen g m => LSystem -> g -> ModuleContext -> m (Production, Env)
findProduction system gen context =
  let defines = view lsysDefines system
      productions = view lsysProductions system
   in let buildEnv x = defines <> envFromContext x context
       in case filter
                 (\rule -> matchProduction defines rule context)
                 productions of
            [x] ->
              return $
              let y = (x, buildEnv x)
               in y
            [] -> return $ (identityProduction context, emptyEnv)
            xs ->
              case partition hasGuard xs of
                ([], xs') -> do
                  i <- uniformRM (0, length xs' - 1) gen
                  return $
                    let x = xs' !! i
                     in (x, buildEnv x)
                (xs', _) -> do
                  i <- uniformRM (0, length xs' - 1) gen
                  return $
                    let x = xs' !! i
                     in (x, buildEnv x)

identityProduction :: ModuleContext -> Production
identityProduction (l, _, _) =
  Production
    { _prodRule =
        makeRule $
        Module {_moduleSymbol = view moduleSymbol l, _moduleParams = []}
    , _prodReplacement = MWord [over moduleParams (map ExprConst) l]
    }

matchProduction :: Env -> Production -> ModuleContext -> Bool
matchProduction globalEnv prod context@(l, pre, post) =
  let r = view prodRule prod
   in view (ruleLetter . moduleSymbol) r == view moduleSymbol l &&
      (case view ruleLetterPre r of
         Nothing -> True
         Just _ ->
           fmap (view moduleSymbol) (view ruleLetterPre r) ==
           fmap (view moduleSymbol) pre) &&
      (case view ruleLetterPost r of
         Nothing -> True
         Just _ ->
           any
             (\p ->
                fmap (view moduleSymbol) (view ruleLetterPost r) ==
                Just (view moduleSymbol p))
             post) &&
      (case view ruleGuard r of
         MatchAll -> True
         MatchGuard op lhs rhs ->
           let env = globalEnv <> envFromContext prod context -- TODO: Add defines
               lhs' = eval env lhs
               rhs' = eval env rhs
            in case op of
                 "=" -> lhs' == rhs'
                 "<" -> lhs' < rhs'
                 ">" -> lhs' > rhs'
                 "<=" -> lhs' <= rhs'
                 ">=" -> lhs' >= rhs'
                 x -> error $ "undefined operator: " <> x)
