-- |
-- Description: Slightly modified brainfuck instruction set
-- Copyright: 2021 Christian Despres
-- License: BSD-2-Clause
-- Maintainer: Christian Despres
--
-- This module re-exports an instruction set with equivalent power to the naive
-- set provided by "Hasbrain.Surface". This set replaces the jumps of the
-- surface language with a single 'Loop' instruction, and gathers runs of
-- increment and decrement instructions into a single 'Add' instruction. The
-- result is better-typed, since we no longer have to worry about maintaining
-- the invariant that in lists of instructions there must be balanced left and
-- right jumps.
--
-- The interpreter for this instruction set is still very direct, but is at
-- least cleaner and more efficient than the one in "Hasbrain.Surface" because
-- of the more convenient instruction set.
module Hasbrain.Loop
  ( -- * The loop instruction set
    Instr (..),
    instrToSurface,
    instrsToSurface,
    parseProgram,

    -- * A basic interpreter
    pureEvalInstrs,
    stepState,
    runStepM,
    MonadInteract (..),
    PureInteract,
    evalPureInteract,
    runPureInteract,
    WordStream (..),
    constStream,
    constStreamWithPrefix,
  )
where

import Data.Word (Word8)
import Hasbrain.InterpreterCommon
  ( MonadInteract (..),
    PureInteract,
    WordStream (..),
    constStream,
    constStreamWithPrefix,
    evalPureInteract,
    initBrainState,
    runPureInteract,
    runStepM,
  )
import Hasbrain.Loop.Instructions
  ( Instr (..),
    instrToSurface,
    instrsToSurface,
  )
import Hasbrain.Loop.Interpreter
  ( stepState,
  )
import Hasbrain.Loop.Parsing
  ( parseProgram,
  )

-- | Evaluate the given instruction list with the given 'Word8' list as its
-- input, extending that input list with a stream of 0 if it is too short.
pureEvalInstrs :: [Instr] -> [Word8] -> [Word8]
pureEvalInstrs instrs inp =
  evalPureInteract (runStepM stepState $ initBrainState instrs) $
    constStreamWithPrefix inp 0
