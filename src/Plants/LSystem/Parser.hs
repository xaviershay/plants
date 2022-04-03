module Plants.LSystem.Parser where

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
    try (parens ((paramParser <* whiteSpace) `sepBy` (char ',' >> whiteSpace))) <|>
    pure []
  return $ Module {_moduleSymbol = symbol, _moduleParams = params}

exprParser = buildExpressionParser table (whiteSpace *> termParser)

termParser =
  (parens exprParser <|> ExprConst <$> decimalParser <|>
   ExprVar <$> many1 exprSymbolParser) <*
  whiteSpace

exprSymbolParser = satisfy (\x -> not $ x `elem` (" (),+-*/^" :: String))

guardParser = do
  lhs <- exprParser
  operator <- foldl1 (<|>) $ map (try . string) ["=", "<=", ">=", "<", ">"]
  rhs <- exprParser

  return $ MatchGuard operator lhs rhs

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

parseGuardUnsafe :: String -> MatchGuard
parseGuardUnsafe = parseUnsafe (guardParser <|> pure MatchAll)
