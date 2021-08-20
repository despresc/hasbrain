module Hasbrain.Surface
  ( -- * The surface-level instruction set
    Instr (..),
    renderInstr,
    renderInstrs,
    stripComments,
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
import Hasbrain.Surface.Instructions
  ( Instr (..),
    renderInstr,
    renderInstrs,
    stripComments,
  )
import Hasbrain.Surface.Interpreter
  ( stepState,
  )
import Hasbrain.Surface.Parsing
  ( parseProgram,
  )

-- | Evaluate the given instruction list with the given 'Word8' list as its
-- input, extending that input list with a stream of 0 if it is too short.
pureEvalInstrs :: [Instr] -> [Word8] -> [Word8]
pureEvalInstrs instrs inp =
  evalPureInteract (runStepM stepState $ initBrainState instrs) $
    constStreamWithPrefix inp 0
