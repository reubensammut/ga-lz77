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

step :: State -> State
step s@(State i o) = case (head i) of
  L x -> State (drop (fromIntegral x) (tail i)) (o ++ take (fromIntegral x) (tail i))
  R x -> State (tail i) (o ++ drop (length o - (fromIntegral x)) o)

evalProg :: State -> State
evalProg = until final step

execute :: Program -> Program
execute (Program i) = Program o
  where (State _ o) = evalProg $ State i []
