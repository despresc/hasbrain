module Hasbrain.Surface
  ( -- * The surface-level instruction set
    Instr (..),
    renderInstr,
    renderInstrs,
    stripComments,
    parseProgram,

    -- * A basic interpreter
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

import Hasbrain.InterpreterCommon
  ( MonadInteract (..),
    PureInteract,
    WordStream (..),
    constStream,
    constStreamWithPrefix,
    evalPureInteract,
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
