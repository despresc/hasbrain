{-# LANGUAGE OverloadedStrings #-}

-- |
-- Description: Direct brainfuck interpreter
-- Copyright: 2021 Christian Despres
-- License: BSD-2-Clause
-- Maintainer: Christian Despres
--
-- A stepped interpreter for the modified brainfuck instruction set in
-- "Hasbrain.Loop.Instructions", translating the operation of the brainfuck VM
-- very directly
module Hasbrain.Loop.Interpreter where

import Data.Traversable (for)
import Hasbrain.InterpreterCommon
import Hasbrain.Loop.Instructions

-- | Run a single instruction from the given 'BrainState'. This counts a 'Loop'
-- as a single instruction, notably.
stepState :: MonadInteract m => BrainState Instr -> m (Maybe (BrainState Instr))
stepState bs = for (popInstrRight $ brainInstrs bs) $ \(ci, bis) ->
  let bsL = bs {brainInstrs = putInstrLeft ci bis}
   in case ci of
        PointerRight -> pure $ bsL {brainData = dataRight $ brainData bs}
        PointerLeft -> pure $ bsL {brainData = dataLeft $ brainData bs}
        Add n -> pure $ bsL {brainData = dataAdd n $ brainData bs}
        Output -> do
          putByte $ readData $ brainData bs
          pure bsL
        Input -> do
          w <- getByte
          pure $ bsL {brainData = setData w $ brainData bs}
        Loop x -> runLoop $ bs {brainInstrs = initInstrTape x}
          where
            runLoop loopBS
              | readData (brainData loopBS) == 0 = pure bsL
              | otherwise = do
                loopBS' <- runStepM stepState loopBS
                if readData (brainData loopBS') == 0
                  then pure $ loopBS' {brainInstrs = brainInstrs bsL}
                  else runLoop $ loopBS' {brainInstrs = initInstrTape x}
        Comment _ -> pure bsL
