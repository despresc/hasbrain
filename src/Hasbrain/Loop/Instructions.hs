{-# LANGUAGE BangPatterns #-}

module Hasbrain.LoopIntermediate.Instructions where

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

-- | Gather a single new 'Instr' from a list of old 'Syn.Instr'. Note that this
-- function will throw an error if it is given input with unbalanced
-- 'Syn.JumpFromLeft' and 'Syn.JumpFromRight' instructions.
unsafePopFromSurface ::
  Syn.Instr ->
  [Syn.Instr] ->
  (Maybe Instr, [Syn.Instr])
unsafePopFromSurface x xs = case x of
  Syn.PointerRight -> (Just PointerRight, xs)
  Syn.PointerLeft -> (Just PointerLeft, xs)
  Syn.Increment -> gatherVal 1 xs
  Syn.Decrement -> gatherVal 255 xs
  Syn.JumpFromLeft -> accUntilMatch id xs
  Syn.JumpFromRight -> (Nothing, xs)
  Syn.Output -> (Just Output, xs)
  Syn.Input -> (Just Input, xs)
  Syn.Comment t -> (Just $ Comment t, xs)
  where
    gatherVal !acc (Syn.Increment : ys) = gatherVal (acc + 1) ys
    gatherVal !acc (Syn.Decrement : ys) = gatherVal (acc - 1) ys
    gatherVal !acc ys = (Just $ Add acc, ys)
    accUntilMatch acc (y:ys) = case unsafePopFromSurface y ys of
      (Just i, ys') -> accUntilMatch (acc . (i:)) ys'
      (Nothing, ys') -> (Just $ Loop $ acc [], ys')
    accUntilMatch _ []
      = error "unsafePopFromSurface internal error - unmatched JumpFromLeft"

-- | Convert a list of old brainfuck 'Syn.Instr' instructions to the new 'Instr'
-- type. Note that this function will throw an error if it is given input with
-- unbalanced 'Syn.JumpFromLeft' and 'Syn.JumpFromRight' instructions.
unsafeFromSurface :: [Syn.Instr] -> [Instr]
unsafeFromSurface (x : xs) = case unsafePopFromSurface x xs of
  (Just i, xs') -> i : unsafeFromSurface xs'
  (Nothing, _) -> error "unsafeFromSurface internal error - unmatched JumpFromRight"
unsafeFromSurface [] = []
