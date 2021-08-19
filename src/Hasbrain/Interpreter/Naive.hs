{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Hasbrain.Interpreter.Naive where

import Data.Traversable (for)
import Hasbrain.Syntax
import Hasbrain.Interpreter.Common

-- | Move right along the tape until just after an unmatched 'JumpFromRight' is
-- encountered, or until the end of the tape is reached
seekRight :: InstrTape Instr -> InstrTape Instr
seekRight = go (0 :: Int)
  where
    go !n it = case moveR it of
      Just (JumpFromLeft, it') -> go (n + 1) it'
      Just (JumpFromRight, it')
        | n == 0 -> it'
        | otherwise -> go (n - 1) it'
      Just (_, it') -> go n it'
      Nothing -> it
    moveR it = case popInstrRight it of
      Just (x, it') -> Just (x, putInstrLeft x it')
      Nothing -> Nothing

-- | Move left along the tape until just before an unmatched 'JumpFromLeft' is
-- encountered, or until the beginning of the tape is reached
seekLeft :: InstrTape Instr -> InstrTape Instr
seekLeft = go (0 :: Int)
  where
    -- Necessarily implemented a little differently from seekRight, though a
    -- slightly more complicated function could work for both
    go !n it = case moveL it of
      Just (JumpFromRight, it') -> go (n + 1) it'
      Just (JumpFromLeft, it')
        | n == 0 -> it -- NOT it'
        | otherwise -> go (n - 1) it'
      Just (_, it') -> go n it'
      Nothing -> it
    moveL it = case popInstrLeft it of
      Just (x, it') -> Just (x, putInstrRight x it')
      Nothing -> Nothing

stepState :: MonadInteract m => BrainState Instr -> m (Maybe (BrainState Instr))
stepState bs = for (popInstrRight $ brainInstrs bs) $ \(ci, bis) ->
  let bsL = bs {brainInstrs = putInstrLeft ci bis}
   in case ci of
        PointerRight -> pure $ bsL {brainData = dataRight $ brainData bs}
        PointerLeft -> pure $ bsL {brainData = dataLeft $ brainData bs}
        Increment -> pure $ bsL {brainData = dataInc $ brainData bs}
        Decrement -> pure $ bsL {brainData = dataDec $ brainData bs}
        Output -> do
          putByte $ readData $ brainData bs
          pure bsL
        Input -> do
          w <- getByte
          pure $ bsL {brainData = setData w $ brainData bs}
        JumpFromLeft
          | readData (brainData bs) == 0 ->
            pure $ bs {brainInstrs = seekRight $ putInstrLeft ci bis}
          | otherwise ->
            pure bsL
        JumpFromRight
          | readData (brainData bs) /= 0 ->
            pure $ bs {brainInstrs = seekLeft $ putInstrRight ci bis}
          | otherwise ->
            pure bsL
        Comment _ -> pure bsL
