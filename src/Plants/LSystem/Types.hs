module Plants.LSystem.Types where

import Data.List (intercalate)
import Data.String (IsString(..))

data Term2 = Sum | Product | Fraction | Exponent
  deriving (Show, Eq)

data Expr =
  ExprConst Double
  | ExprVar String
  | ExprOp2 Term2 Expr Expr
  deriving (Show, Eq)

data PLetter a = PLetter {
  letterSymbol :: String,
  letterParams :: [a]
} deriving (Eq)

instance Show a => Show (PLetter a) where
  show (PLetter { letterSymbol = s, letterParams = ps }) = s <> if null ps then "" else show ps

type Letter = PLetter Double
type LetterExpr = PLetter Expr
type LetterPattern = PLetter String

data LWord a = LWord [a]
  deriving (Eq)

instance Semigroup (LWord x) where
  (LWord a) <> (LWord b) = LWord $ a <> b

instance Monoid (LWord a) where
  mempty = LWord []

instance Show a => Show (LWord a) where
  show (LWord l) = intercalate " " $ map show l
