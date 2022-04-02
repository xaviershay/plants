{-# LANGUAGE OverloadedStrings #-}

module Plants.LSystem.Eval where

import Plants.LSystem.Types
import Plants.Prelude

import Control.Lens (assign, makeLenses, over, set, view)
import qualified Data.HashMap.Strict as M
import Data.List (partition, tails)
import System.Random.Stateful (StatefulGen, mkStdGen, runStateGen, uniformRM)

type ModuleContext = ((ModuleFixed, Maybe ModuleFixed), Maybe ModuleFixed)

-- data Tree a = Root [Tree a] | Node a [Tree a] deriving (Show)

-- "ABC" -> Node 'A' [Node 'B' [Node 'C']]
-- "[A]" -> Node 'A' []
-- "[AB]C" -> Root [Node 'A' [Node 'B' []]
-- "A[B]C" -> Node 'A' [Node 'B' [], Node 'C' []]
-- "A[[B]C]D" -> Node 'A' [Node 'B' [], Node 'C' [], Node 'D' []]
-- "A[B][C]" -> Node 'A' [Node 'B' [], Node 'C' []]
-- "A[BD][C]" -> Node 'A' [Node 'B' [Node 'D' []], Node 'C' []]


-- t1 = foldl build [Root []] ("ABC" :: String)
-- --t2 = foldl build [Root []] ("A[B]C" :: String)
-- 
-- build tree '[' = (Root []:tree)
-- build (tree:Root ls:ts) ']' = (Root (tree:ls):ts)
-- build tree ']' = error $ "unhandled case: " <> show tree
-- build ts x = (Node x []:ts)

-- t2 = runState builder "ABC"
-- 
-- builder = do
--   char <- get
-- 
--   case char of
--     "" -> Nil
--     ('[':xs)
-- data Crumb =
--     LeftCrumb Tree
--   | RightCrumb Tree
--   deriving (Show)
-- 
-- type Crumbs = [Crumb]
-- type Zipper = (Tree, Crumbs)
-- 
-- goTop :: Zipper -> Tree a
-- goTop = undefined
-- 
-- toTree :: String -> Tree Char
-- toTree = goTop $ toTree' (Nil, [])
-- 
-- toTree' :: Zipper -> String -> Zipper
-- toTree' zipper (x:xs) = toTree' (setNode x $ goLeft zipper) xs
-- toTree' (Node a lhs rhs) ("[":xs) = (Node a lhs (toTree xs))
-- toTree' node ("]":xs) = node
-- 
-- setNode :: a -> Zipper -> Zipper
-- setNode x (Nil, crumbs) = (Node x Nil Nil)

-- parent (skipping ignores)
-- 
-- nextChild or sibling

step :: StatefulGen g m => LSystem -> g -> m LSystem
step system gen = do
  let MWord axiom = view lsysAxiom system
  let ignores = view lsysIgnore system
  let axiomWithContext =
        zip (zip axiom (extractPres axiom ignores)) (extractPosts axiom ignores)
  parts <- mapM (findProduction system gen) axiomWithContext
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

extractPosts :: [ModuleFixed] -> [ModuleFixed] -> [Maybe ModuleFixed]
extractPosts word ignores =
  let f =
        filter
          (\x ->
             not $ view moduleSymbol x `elem` (map (view moduleSymbol) ignores))
   in map (headMaybe . f) . drop 1 . tails $ word
extractPosts' transform word ignores =
  let f =
        filter
          (\x ->
             not $ view moduleSymbol x `elem` (map (view moduleSymbol) ignores))
   in map (firstSymbolExcludingBrackets (transform . view moduleSymbol) . f) .
      drop 1 . tails $
      word
-- and r is a subtree
-- of T originating at the ending node of S. The production can then be
-- applied by replacing S with the axial tree specified as the production
-- successor.
firstSymbolExcludingBrackets :: (a -> String) -> [a] -> Maybe a
firstSymbolExcludingBrackets f xs = go 0 (map (\x -> (f x, x)) xs)
  where
    go :: Int -> [(String, a)] -> Maybe a
    go n (("[", _):xs) = go (n + 1) xs
    go n (("]", _):xs) = go (n - 1) xs
    go n ((_, x):_)
      | n <= 0 = Just x
    go n (_:xs) = go n xs
    go n [] = Nothing

extractPres :: [ModuleFixed] -> [ModuleFixed] -> [Maybe ModuleFixed]
extractPres word = reverse . extractPosts' flipBrackets (reverse word)

flipBrackets "]" = "["
flipBrackets "[" = "]"
flipBrackets x = x

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
envFromContext p ((l1, l2), l3) =
  Env . M.fromList $
  envForLetter (Just $ view ruleLetter rule) (Just l1) <>
  envForLetter (view ruleLetterPre rule) l2 <>
  envForLetter (view ruleLetterPost rule) l3
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
identityProduction ((l, _), _) =
  Production
    { _prodRule =
        makeRule $
        Module {_moduleSymbol = view moduleSymbol l, _moduleParams = []}
    , _prodReplacement = MWord [over moduleParams (map ExprConst) l]
    }

matchProduction :: Env -> Production -> ModuleContext -> Bool
matchProduction globalEnv prod context@((l, pre), post) =
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
           fmap (view moduleSymbol) (view ruleLetterPost r) ==
           fmap (view moduleSymbol) post) &&
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
