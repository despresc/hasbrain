module Hasbrain.Surface
  ( -- * The surface-level instruction set
    Instr (..),
    renderInstr,
    renderInstrs,
    stripComments,

    -- * A basic interpreter
    stepState,
    runStepM,
    MonadInteract(..),
    PureInteract,
    runPureInteract,
    WordStream(..),
    constStream,
    constStreamWithPrefix,

  )
where

import Hasbrain.InterpreterCommon
  ( WordStream (..),
    constStream,
    constStreamWithPrefix,
    runStepM,
    MonadInteract(..),
    PureInteract,
    runPureInteract,
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
