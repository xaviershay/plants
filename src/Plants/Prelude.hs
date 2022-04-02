module Plants.Prelude
  ( module Plants.Prelude
  , module Debug.Trace
  , module Data.List
  ) where

import Data.List (intercalate)
import Debug.Trace (trace, traceM)
import Numeric (showFFloat)

headMaybe (x:_) = Just x
headMaybe [] = Nothing

toRadians a = a / 180.0 * pi

traceS x = trace (show x) x

showFullPrecision :: Double -> String
showFullPrecision x = showFFloat Nothing x ""
