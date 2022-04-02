module Plants.Prelude where

headMaybe (x:_) = Just x
headMaybe [] = Nothing
