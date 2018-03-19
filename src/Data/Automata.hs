module Data.Automata
    ( Automata(..)
    ) where

import Data.Hashable

class Automata a where
  isAccepted :: (Hashable b, Hashable c, Eq b, Eq c) => a b c -> Bool
  delta :: (Hashable b, Hashable c, Eq b, Eq c) => a b c -> c -> a b c

  deltaHat :: (Hashable b, Hashable c, Eq b, Eq c) => a b c -> [c] -> a b c
  deltaHat = foldl delta

  {-# MINIMAL isAccepted, delta #-}


