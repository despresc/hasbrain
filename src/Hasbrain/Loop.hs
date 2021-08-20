module Hasbrain.Loop
  ( -- * The loop instruction set
    Instr (..),
    instrToSurface,
    instrsToSurface,

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
import Hasbrain.Loop.Instructions
  ( Instr (..),
    instrToSurface,
    instrsToSurface,
  )
import Hasbrain.Surface.Interpreter
  ( stepState,
  )
