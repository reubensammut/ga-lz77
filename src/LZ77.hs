module LZ77
    ( Instruction(..),
      Program(..),
      execute
    ) where

import Data.Natural
import Data.List (null)

data Instruction
  = R Natural
  | L Natural
  deriving Show

type Inputs = [Instruction]
type Outputs = [Instruction]

newtype Program = Program { getProg :: [Instruction] }
  deriving Show

data State = State Inputs Outputs

final :: State -> Bool
final (State i _ ) = null i

takeLast :: Int -> [a] -> [a]
takeLast x ls = drop (length ls - x) ls

step :: State -> State
step (State i o) = case (head i) of
  L x -> let i' = drop (fromIntegral x) (tail i)
             o' = take (fromIntegral x) (tail i)
         in State i' (o ++ o')
  R x -> let i' = tail i
             o' = takeLast (fromIntegral x) o
         in State i' (o ++ o')

evalProg :: State -> State
evalProg = until final step

execute :: Program -> Program
execute (Program i) = Program o
  where (State _ o) = evalProg $ State i []
