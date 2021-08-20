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
