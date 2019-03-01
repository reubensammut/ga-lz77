module GA
    ( GA(..)
    , select
    ) where

import System.Random

class GA a where
  genRandom :: RandomGen g => g -> (a, g)
  getFitness :: a -> Float
  mate :: a -> a -> a
  mutate :: RandomGen g => g -> Float -> a -> (a, g)

average :: Fractional a => [a] -> a
average l = sum l / fromIntegral (length l)

select :: GA a => [a] -> [a]
select pop = map snd . filter (\(a,_) -> a > avg) $ fitnesses
  where fitnesses = flip zip pop . map getFitness $ pop
        avg = average . map (fst) $ fitnesses

