module Plants.Prelude (module Plants.Prelude, module Debug.Trace) where

import Debug.Trace (trace, traceM)

headMaybe (x:_) = Just x
headMaybe [] = Nothing

toRadians a = a / 180.0 * pi

traceS x = trace (show x) x
