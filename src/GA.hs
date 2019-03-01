module GA where

import System.Random

class (Random a) => GA a where
  genRandom :: RandomGen g => g -> a
  getFitness :: a -> Float
  mate :: a -> a -> a
  mutate :: RandomGen g => g -> Float -> a -> a
