module Plants.Prelude
  ( module Plants.Prelude
  , module Debug.Trace
  , module Data.List
  , module Control.Lens
  , module Control.Applicative
  ) where

import Data.List (intercalate)
import Debug.Trace (trace, traceM)
import Numeric (showFFloat)
import Control.Lens (view, set)
import Control.Applicative ((<|>))

headMaybe (x:_) = Just x
headMaybe [] = Nothing

toRadians a = a / 180.0 * pi

traceS x = trace (show x) x

showFullPrecision :: Double -> String
showFullPrecision x = showFFloat Nothing x ""
