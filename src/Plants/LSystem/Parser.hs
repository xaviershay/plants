module Plants.LSystem.Parser (parseWordUnsafe, parseWordExprUnsafe, parsePatternUnsafe) where

import Plants.LSystem.Types

import Text.Parsec
import Text.Parsec.Expr
import qualified Text.Parsec.Token    as Tok
import qualified Text.Parsec.Language as Tok

lexer :: Tok.TokenParser ()
lexer = Tok.makeTokenParser style
  where
    style = Tok.emptyDef
      { Tok.reservedOpNames = ["+", "^", "/", "*"]
      , Tok.reservedNames   = []
      , Tok.identStart      = letter
      }

reservedOp = Tok.reservedOp lexer
prefix name fun = Prefix (do { reservedOp name; return fun })
binary name fun assoc = Infix (do { reservedOp name; return fun}) assoc

table = [ [ prefix "-" (ExprOp2 Product (ExprConst (-1)))]
        , [ binary "^" (ExprOp2 Exponent) AssocLeft ]
        , [ binary "*" (ExprOp2 Product) AssocLeft, binary "/" (ExprOp2 Fraction) AssocLeft]
        , [ binary "+" (ExprOp2 Sum) AssocLeft
          , binary "-" (\a b -> ExprOp2 Sum a (ExprOp2 Product b (ExprConst (-1)))) AssocLeft
          ]
        ]
parens = between (char '(' <* whiteSpace) (char ')')
termExpr = (parens exprParser
             <|> ExprConst <$> paramExpr
             <|> ExprVar <$> many1 exprSymbolExpr
           ) <* whiteSpace
paramExpr = do
  leading <- many1 digit
  decimal <- optionMaybe (char '.' >> many1 digit)

  let trailing = case decimal of
                   Just x -> "." <> x
                   Nothing -> ""

  return $ read (leading <> trailing)
exprSymbolExpr = satisfy (\x -> not $ x `elem` (" (),+-*/^" :: String))
whiteSpace = Tok.whiteSpace lexer
patternExpr = many1 symbolExpr

symbolExpr = satisfy (\x -> not $ x `elem` (" ()," :: String))
letterExpr paramParser = do
  symbol <- many1 symbolExpr
  params <- try (char '(' >> whiteSpace) *> (paramParser <* whiteSpace) `sepBy` (char ',' >> whiteSpace) <* (char ')') <|> pure []

  return $ PLetter {
    letterSymbol = symbol,
    letterParams = params
  }

exprParser = buildExpressionParser table (whiteSpace *> termExpr)
pwordPatternExpr = whiteSpace *> letterExpr patternExpr <* whiteSpace
pwordExprExpr = LWord <$> (whiteSpace *> many (letterExpr exprParser <* whiteSpace))

pwordExpr = LWord <$> (whiteSpace *> many1 (letterExpr paramExpr <* whiteSpace))

parseWordUnsafe :: String -> LWord Letter
parseWordUnsafe input =
  case parse pwordExpr input input of
    Right x -> x
    Left x -> error $ show x

parsePatternUnsafe :: String -> LetterPattern
parsePatternUnsafe input =
  case parse pwordPatternExpr input input of
    Right x -> x
    Left x -> error $ show x

parseWordExprUnsafe :: String -> LWord LetterExpr
parseWordExprUnsafe input =
  case parse pwordExprExpr input input of
    Right x -> x
    Left x -> error $ show x
