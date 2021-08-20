module Hasbrain.Loop.Instructions where

import Data.Text (Text)
import Data.Word (Word8)
import qualified Hasbrain.Surface.Instructions as Syn

{-
The brainfuck instruction set is modified here to contain a Loop and an Inc
construct, replacing the previous increment, decrement, jump from left, and jump
from right instructions.

Our technique here uses the fact that we are interpreting arithmetic as
occurring modulo 256 (i.e., over/underflow is not an error).
-}

data Instr
  = PointerRight
  | PointerLeft
  | Add !Word8
  | Loop [Instr]
  | Output
  | Input
  | Comment !Text
  deriving (Eq, Ord, Show)

instrToSurface :: Instr -> [Syn.Instr]
instrToSurface PointerRight = [Syn.PointerRight]
instrToSurface PointerLeft = [Syn.PointerLeft]
instrToSurface (Add x)
  | x < 128 = replicate (fromIntegral x) Syn.Increment
  | otherwise = replicate (256 - fromIntegral x) Syn.Decrement
instrToSurface (Loop instrs) =
  Syn.JumpFromLeft : (instrsToSurface instrs <> [Syn.JumpFromRight])
instrToSurface Output = [Syn.Output]
instrToSurface Input = [Syn.Input]
instrToSurface (Comment t) = [Syn.Comment t]

instrsToSurface :: [Instr] -> [Syn.Instr]
instrsToSurface = concatMap instrToSurface
