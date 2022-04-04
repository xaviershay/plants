{-# LANGUAGE OverloadedStrings #-}

module Plants.LSystem.Eval where

import Plants.LSystem.Types
import Plants.Prelude

import Control.Lens (assign, makeLenses, over, set, view)
import qualified Data.HashMap.Strict as M
import Data.Foldable (toList)
import Data.List (partition, tails)
import Data.Maybe (listToMaybe)
import System.Random.Stateful (StatefulGen, mkStdGen, runStateGen, uniformRM)

data TreeBuilder a
  = Child a
  | Sibling (Tree a)
  deriving (Show)

type ModuleContext = (ModuleFixed, Maybe ModuleFixed, [ModuleFixed])

step :: StatefulGen g m => LSystem -> g -> m LSystem
step system gen = do
  let MWord axiom = view lsysAxiom system
  let ignores = view lsysIgnore system
  let axiomContext = axiomWithContext ignores (toList axiom)
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
   in MWord . fmap f $ replacementWord
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

emptyEnv = Env mempty

hasGuard :: (Production, Env) -> Bool
hasGuard = (/= MatchAll) . view (prodRule . ruleGuard) . fst

findProduction ::
     StatefulGen g m => LSystem -> g -> ModuleContext -> m (Production, Env)
findProduction system gen context =
  let defines = view lsysDefines system
      productions = view lsysProductions system
   in case concatMap (\rule -> matchProduction defines rule context) productions of
        [(x, e)] -> return $ (x, e)
        [] -> return $ (identityProduction context, emptyEnv)
        xs ->
          case partition hasGuard xs of
            ([], xs') -> do
              i <- uniformRM (0, length xs' - 1) gen
              return $ xs' !! i
            (xs', _) -> do
              i <- uniformRM (0, length xs' - 1) gen
              return $ xs' !! i

identityProduction :: ModuleContext -> Production
identityProduction (l, _, _) =
  Production
    { _prodRule =
        makeRule $
        Module {_moduleSymbol = view moduleSymbol l, _moduleParams = []}
    , _prodReplacement = mwordFromList [over moduleParams (map ExprConst) l]
    }

matchProduction :: Env -> Production -> ModuleContext -> [(Production, Env)]
matchProduction globalEnv prod context@(l, pre, post) =
  map ((,) prod) . filter satisfiesGuard $ candidateEnvs
  where
    r = view prodRule prod
    postEnvs :: [Env]
    postEnvs =
      case view ruleLetterPost r of
        Nothing -> [globalEnv]
        Just m ->
          concatMap
            (\p ->
               if view moduleSymbol m == view moduleSymbol p
                 then [globalEnv <> envForLetter (Just m) (Just p)]
                 else [])
            post
    candidateEnvs =
      if (view (ruleLetter . moduleSymbol) r == view moduleSymbol l &&
          (case view ruleLetterPre r of
             Nothing -> True
             Just pm ->
               Just (view moduleSymbol pm) == fmap (view moduleSymbol) pre))
        then map
               (\e ->
                  e <> envForLetter (view ruleLetterPre r) pre <>
                  envForLetter (Just $ view ruleLetter r) (Just l))
               postEnvs
        else []
    satisfiesGuard env =
      case view ruleGuard r of
        MatchAll -> True
        MatchGuard op lhs rhs ->
          let lhs' = eval env lhs
              rhs' = eval env rhs
           in case op of
                "=" -> lhs' == rhs'
                "<" -> lhs' < rhs'
                ">" -> lhs' > rhs'
                "<=" -> lhs' <= rhs'
                ">=" -> lhs' >= rhs'
                x -> error $ "undefined operator: " <> x
    envForLetter :: Maybe ModulePattern -> Maybe ModuleFixed -> Env
    envForLetter pat l =
      case (pat, l) of
        (Just p', Just l') ->
          if view moduleSymbol p' == view moduleSymbol l'
            then let paramLabels = view moduleParams p'
                     paramValues = map ExprConst . (view moduleParams) $ l'
                  in Env . M.fromList $ zip paramLabels paramValues
            else mempty
        _ -> mempty

gatherContext ignores m parents children =
  (m, listToMaybe . filter (not . ignored) $ parents, immediates children)
  where
    ignored x =
      view moduleSymbol x `elem` ("[" : "]" : map (view moduleSymbol) ignores)
    immediates = concatMap f
    f (Root xs) = immediates xs
    f (Node x cs) =
      if ignored x
        then immediates cs
        else [x]

type TreeTraverser a b = a -> [a] -> [Tree a] -> b

mapTree :: TreeTraverser a b -> Tree a -> [b]
mapTree f t = mapTree' mempty t
  where
    mapTree' parent (Root cs) = concatMap (mapTree' parent) . reverse $ cs
    mapTree' parent (Node x cs) =
      (f x parent $ reverse cs) : mapTree' (x : parent) (Root cs)

buildTreeWith :: (a -> String) -> [a] -> Tree a
buildTreeWith mapper = fst . buildTreeWith'
  where
    builderToTree ns (Child x:rest) = (Node x (builderToTree [] rest) : ns)
    builderToTree ns (Sibling x:rest) = builderToTree (x : ns) rest
    builderToTree tree [] = tree
    buildTreeWith' ms =
      let (treeInstructions, remainder) = extractTree ms
       in (Root $ builderToTree [] treeInstructions, remainder)
    extractTree [] = ([], [])
    extractTree (m:rest) =
      case mapper m of
        "[" ->
          let (subtree, rest') = buildTreeWith' rest
              (ts, rest'') = extractTree rest'
           in ([Sibling (Node m [subtree])] <> ts, rest'')
        "]" -> ([Child m], rest)
        x ->
          let (ts, rest') = extractTree rest
           in ([Child m] <> ts, rest')

-- TODO: Maybe adapt to use seq rather than list so don't switch types
axiomWithContext ignores =
  mapTree (gatherContext ignores) . buildTreeWith (view moduleSymbol)
