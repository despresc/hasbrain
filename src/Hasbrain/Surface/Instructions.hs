{-# LANGUAGE OverloadedStrings #-}

module Hasbrain.Surface.Instructions where

import Data.Text (Text)

{-
The brainfuck instruction set:

> | increment data pointer
< | decrement data pointer
+ | increment data pointer value
- | decrement data pointer value
. | output data pointer value
, | accept one byte of input and store in the data pointer
[ | if the data pointer value is zero, jump to the instruction after the matching ]
] | if the data pointer value is nonzero, jump to the instruction after the matching [

All other characters are uninterpreted. We treat runs of such characters as
comments here.
-}

data Instr
  = PointerRight
  | PointerLeft
  | Increment
  | Decrement
  | JumpFromLeft
  | JumpFromRight
  | Output
  | Input
  | Comment !Text
  deriving (Eq, Ord, Show)

renderInstr :: Instr -> Text
renderInstr PointerRight = ">"
renderInstr PointerLeft = "<"
renderInstr Increment = "+"
renderInstr Decrement = "-"
renderInstr Output = "."
renderInstr Input = ","
renderInstr JumpFromLeft = "["
renderInstr JumpFromRight = "]"
renderInstr (Comment t) = t

renderInstrs :: [Instr] -> Text
renderInstrs = foldMap renderInstr

stripComments :: [Instr] -> [Instr]
stripComments = filter $ \x -> case x of
  Comment _ -> False
  _ -> True
