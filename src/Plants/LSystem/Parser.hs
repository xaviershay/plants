module Plants.LSystem.Parser
  ( parseWordUnsafe
  , parseWordExprUnsafe
  , parseExprUnsafe
  , parsePatternUnsafe
  , testP
  ) where

import Plants.LSystem.Types
import Plants.Prelude

import Debug.Trace
import Text.Parsec
import Text.Parsec.Expr
import qualified Text.Parsec.Language as Tok
import qualified Text.Parsec.Token as Tok

lexer :: Tok.TokenParser ()
lexer = Tok.makeTokenParser style
  where
    style =
      Tok.emptyDef
        { Tok.reservedOpNames = ["+", "^", "/", "*"]
        , Tok.reservedNames = []
        , Tok.identStart = letter
        }

reservedOp = Tok.reservedOp lexer

prefix name fun =
  Prefix
    (do reservedOp name
        return fun)

binary name fun assoc =
  Infix
    (do reservedOp name
        return fun)
    assoc

table =
  [ [prefix "-" (ExprOp2 Product (ExprConst (-1)))]
  , [binary "^" (ExprOp2 Exponent) AssocLeft]
  , [ binary "*" (ExprOp2 Product) AssocLeft
    , binary "/" (ExprOp2 Fraction) AssocLeft
    ]
  , [ binary "+" (ExprOp2 Sum) AssocLeft
    , binary
        "-"
        (\a b -> ExprOp2 Sum a (ExprOp2 Product b (ExprConst (-1))))
        AssocLeft
    ]
  ]


testP =
  let MWord x = parseWordUnsafe "A [ B D ] [ C ]"
   in trace (show $ mapTree gatherContext $ buildTreeWith (view moduleSymbol) $ x) x

data TreeBuilder a
  = Child a
  | Sibling (Tree a)
  deriving (Show)

gatherContext m parents children = (m, take 1 parents, immediates children)
  where
    immediates = concatMap f2
    f2 (Root xs) = immediates xs
    f2 (Node x _) = [x]

type TreeTraverser a b = a -> [a] -> [Tree a] -> b

mapTree :: TreeTraverser a b -> Tree a -> [b]
mapTree f t = mapTree' mempty t
  where
    mapTree' parent (Root cs) = concatMap (mapTree' parent) . reverse $ cs
    mapTree' parent (Node x cs) = (f x parent $ reverse cs) : mapTree' (x:parent) (Root cs)

buildTreeWith :: (a -> String) -> [a] -> Tree a
buildTreeWith mapper = fst . blah'

  where
    f ns (Child x:rest) = (Node x (f [] rest) : ns)
    f ns (Sibling x:rest) = f (x : ns) rest
    f tree [] = tree

    blah' ms = let (treeInstructions, remainder) = extractTree ms in
      (Root $ f [] treeInstructions, remainder)

    extractTree [] = ([], [])
    extractTree (m:remainder) = case mapper m of
                      "[" -> let (ts, rest) = blah' remainder in let (x, y) = extractTree rest in ([Sibling ts] <> x, y)
                      "]" -> ([], remainder)
                      x -> let (ts, rest) = extractTree remainder in ([Child m] <> ts, rest)

-- moduleParser2 paramParser = do
--   symbol <- many1 symbolParser2
--   params <-
--     try (parens (paramParser <* whiteSpace) `sepBy` (char ',' >> whiteSpace)) <|>
--     pure []
--   _ <- whiteSpace
--   return $ Module {_moduleSymbol = symbol, _moduleParams = params}
-- 
-- wordParser modParser = do
--   _ <- whiteSpace
--   trees <-
--     many1
--       ( Child <$> modParser <|>
--        (Sibling <$> (char '[' *> wordParser modParser <* char ']' <* whiteSpace)))
--   return . Root $ f [] trees
--   where
--     f :: [Tree a] -> [X a] -> [Tree a]
--     f ns (Child x:rest) = (Node x (f [] rest) : ns)
--     f ns (Sibling x:rest) = f (x : ns) rest
--     f tree [] = tree
-- 
-- symbolParser2 = do
--   symbol <- satisfy (\x -> not $ x `elem` (" (),[]" :: String))
--   return $ symbol

-- "ABC" -> Node 'A' [Node 'B' [Node 'C']]
-- "[A]" -> Node 'A' []
-- "[AB]C" -> Root [Node 'A' [Node 'B' []]
-- "A[B]C" -> Node 'A' [Node 'B' [], Node 'C' []]
-- "A[[B]C]D" -> Node 'A' [Node 'B' [], Node 'C' [], Node 'D' []]
-- "A[B][C]" -> Node 'A' [Node 'B' [], Node 'C' []]
-- "A[BD][C]" -> Node 'A' [Node 'B' [Node 'D' []], Node 'C' []]
-- testP =
--   toS . normT $
--   let x = parseUnsafe (wordParser $ moduleParser2 symbolParser2) "Z X Y [ A ] [ B D [ E ] ] [ C ]"
--    in trace (show $ mapTree2 testF $ normT x) x
-- 
-- A B [Node 'A' [Node 'B' []]]
-- F B [Node 'F' [Node 'B' []]]
-- A -> F G
-- F G B [Node 'F' [Node 'G' [Node 'B' []]]]
-- A [ B ] C [Node 'A' [Node 'C' [], Node 'B' []]]
-- 
-- 
-- F G [ B ] C [Node 'F' [Node 'G' [Node 'C' [], Node 'B' []]]]
-- F [ G ] [ B ] C [Node 'F' [Node 'C' [], Node 'B' [], Node 'G' []]]
-- 
-- replacement = [Node 'F' [Node 'G' []]]
-- replacement = [Node 'F' [Node 'G' []]]

-- mapTree2 :: TreeTraverser a b -> Tree a -> Tree b
-- mapTree2 f t = mapTree2' mempty t
--   where
--     --mapTree2' parent (Root cs) = concatMap (mapTree2' parent) . reverse $ cs
--     mapTree2' parent (Node x cs) = Node (f x parent $ reverse cs) (map (mapTree2' (x:parent)) cs)

normT (Root [x]) = normT x
normT (Root xs) = Root $ map normT xs
normT (Node a ns) = Node a (map normT ns)

-- toS :: Show a => Tree (Module a) -> String
-- toS = intercalate " " . map show . toListT moduleBracket
-- 
-- moduleBracket = (\x -> [lbracket] <> toListT moduleBracket x <> [rbracket])
-- 
-- toListT :: (Tree a -> [a]) -> Tree a -> [a]
-- toListT f (Root [c]) = toListT f c
-- toListT f (Root cs) =
--   concatMap f . reverse $ cs
-- toListT f (Node n []) = [n]
-- toListT f (Node n [c]) = [n] <> toListT f c
-- toListT f (Node n cs) = (n:toListT f (Root cs))
-- 
-- lbracket = Module { _moduleSymbol = "[", _moduleParams = mempty }
-- rbracket = Module { _moduleSymbol = "]", _moduleParams = mempty }

parens = between (char '(' <* whiteSpace) (char ')')

decimalParser = do
  leading <- many1 digit
  decimal <- optionMaybe (char '.' >> many1 digit)
  let trailing =
        case decimal of
          Just x -> "." <> x
          Nothing -> ""
  return $ read (leading <> trailing)

whiteSpace = Tok.whiteSpace lexer

symbolParser = satisfy (\x -> not $ x `elem` (" ()," :: String))

moduleParser paramParser = do
  symbol <- many1 symbolParser
  params <-
    try (parens (paramParser <* whiteSpace) `sepBy` (char ',' >> whiteSpace)) <|>
    pure []
  return $ Module {_moduleSymbol = symbol, _moduleParams = params}

exprParser = buildExpressionParser table (whiteSpace *> termParser)

termParser =
  (parens exprParser <|> ExprConst <$> decimalParser <|>
   ExprVar <$> many1 exprSymbolParser) <*
  whiteSpace

exprSymbolParser = satisfy (\x -> not $ x `elem` (" (),+-*/^" :: String))

parseUnsafe :: Parsec SourceName () a -> SourceName -> a
parseUnsafe parser input =
  case parse parser input input of
    Right x -> x
    Left x -> error $ show x

parseWordUnsafe :: String -> MWord ModuleFixed
parseWordUnsafe =
  parseUnsafe $
  MWord <$> (whiteSpace *> many1 (moduleParser decimalParser <* whiteSpace))

parsePatternUnsafe :: String -> ModulePattern
parsePatternUnsafe =
  parseUnsafe $ whiteSpace *> moduleParser (many1 symbolParser) <* whiteSpace

parseExprUnsafe :: String -> Expr
parseExprUnsafe = parseUnsafe exprParser

parseWordExprUnsafe :: String -> MWord ModuleExpr
parseWordExprUnsafe =
  parseUnsafe $
  MWord <$> (whiteSpace *> many (moduleParser exprParser <* whiteSpace))
