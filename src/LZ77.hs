{-# LANGUAGE InstanceSigs #-}

module LZ77
    ( InstructionType(..),
      Instruction(..),
      Program(..),
      execute
    ) where

import Data.Natural
import Data.List (null)
import System.Random
import GA

data InstructionType
  = L
  | R
  deriving (Eq, Show, Bounded, Enum)

instance Random InstructionType where
  randomR (lo, hi) gen
    = let (a, g') = randomR (fromEnum lo, fromEnum hi) gen
      in (toEnum a, g')
  random = randomR (minBound, maxBound)

data Instruction = Instruction InstructionType Natural
  deriving (Eq, Show)

instance Random Instruction where
  randomR (Instruction tl il, Instruction th ih) gen
    = let (t', g')  = randomR (tl, th) gen
          (i', g'') = randomR (toInteger il, toInteger ih) g'
      in (Instruction t' (fromIntegral i'), g'')
  random gen
    = let (t, g')  = random gen
          (i, g'') = randomR (0, maxBound :: Int) g'
      in (Instruction t (fromIntegral i), g'')

type Inputs = [Instruction]
type Outputs = [Instruction]

newtype Program = Program { getProg :: [Instruction] }
  deriving (Eq, Show)

data State = State Inputs Outputs

final :: State -> Bool
final (State i _ ) = null i

takeLast :: Int -> [a] -> [a]
takeLast x ls = drop (length ls - x) ls

step :: State -> State
step (State i o) = case (head i) of
  Instruction L x -> let i' = drop (fromIntegral x) (tail i)
                         o' = take (fromIntegral x) (tail i)
                     in State i' (o ++ o')
  Instruction R x -> let i' = tail i
                         o' = takeLast (fromIntegral x) o
                     in State i' (o ++ o')

evalProg :: State -> State
evalProg = until final step

execute :: Program -> Program
execute (Program i) = Program o
  where (State _ o) = evalProg $ State i []
