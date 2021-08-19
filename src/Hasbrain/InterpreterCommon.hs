{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Hasbrain.InterpreterCommon where

import Control.Monad.RWS.Strict (RWS)
import qualified Control.Monad.RWS.Strict as RWS
import Data.Word (Word8)

-- | An infinite stream of 'Word8' values
data WordStream = WordStream
  { streamHead :: !Word8,
    streamTail :: WordStream
  }

constStream :: Word8 -> WordStream
constStream w = WordStream w $ constStream w

constStreamWithPrefix :: [Word8] -> Word8 -> WordStream
constStreamWithPrefix (x : xs) y = WordStream x $ constStreamWithPrefix xs y
constStreamWithPrefix [] y = constStream y

-- | The instruction tape. The "cursor" exists in the space between
-- instructions; the instruction to its right is the current one.
data InstrTape a = InstrTape [a] [a]
  deriving (Eq, Ord, Show)

-- | The data tape, a two-way infinite stream of 'Word8' values.
data DataTape = DataTape WordStream !Word8 WordStream

-- | The data tape starts with all cells initialized to zero
initCellTape :: DataTape
initCellTape = DataTape zeroes 0 zeroes
  where
    zeroes = constStream 0

initInstrTape :: [a] -> InstrTape a
initInstrTape = InstrTape []

popInstrRight :: InstrTape a -> Maybe (a, InstrTape a)
popInstrRight (InstrTape ls (r : rs)) = Just (r, InstrTape ls rs)
popInstrRight _ = Nothing

popInstrLeft :: InstrTape a -> Maybe (a, InstrTape a)
popInstrLeft (InstrTape (l : ls) rs) = Just (l, InstrTape ls rs)
popInstrLeft _ = Nothing

putInstrRight :: a -> InstrTape a -> InstrTape a
putInstrRight i (InstrTape ls rs) = InstrTape ls (i : rs)

putInstrLeft :: a -> InstrTape a -> InstrTape a
putInstrLeft i (InstrTape ls rs) = InstrTape (i : ls) rs

-- | Move the data pointer right
dataRight :: DataTape -> DataTape
dataRight (DataTape l c (WordStream n w)) = DataTape (WordStream c l) n w

-- | Move the data pointer left
dataLeft :: DataTape -> DataTape
dataLeft (DataTape (WordStream n w) c r) = DataTape w n (WordStream c r)

-- | Increment the current cell
dataInc :: DataTape -> DataTape
dataInc (DataTape l c r) = DataTape l (c + 1) r

-- | Decrement the current cell
dataDec :: DataTape -> DataTape
dataDec (DataTape l c r) = DataTape l (c - 1) r

-- | Read the value of the current cell
readData :: DataTape -> Word8
readData (DataTape _ c _) = c

-- | Set the value of the current cell
setData :: Word8 -> DataTape -> DataTape
setData w (DataTape l _ r) = DataTape l w r

class Monad m => MonadInteract m where
  getByte :: m Word8
  putByte :: Word8 -> m ()

data BrainState a = BrainState
  { brainInstrs :: InstrTape a,
    brainData :: DataTape
  }

initBrainState :: [a] -> BrainState a
initBrainState is = BrainState (initInstrTape is) initCellTape

-- | Run a monadic computation until it halts, returning the final 'Just' value.
runStepM ::
  Monad m => (a -> m (Maybe a)) ->
  a -> m a
runStepM step a = do
  ma <- step a
  case ma of
    Just a' -> runStepM step a'
    Nothing -> pure a

newtype DL a = DL {unDL :: [a] -> [a]}

instance Semigroup (DL a) where
  DL x <> DL y = DL (x . y)

instance Monoid (DL a) where
  mempty = DL id

singletonDL :: a -> DL a
singletonDL = DL . (:)

-- | A pure simulation of input and output
newtype PureInteract a = PureInteract
  {unPureInteract :: RWS () (DL Word8) WordStream a}
  deriving (Functor, Applicative, Monad)

instance MonadInteract PureInteract where
  getByte = PureInteract $ do
    WordStream n w <- RWS.get
    RWS.put w
    pure n
  putByte = PureInteract . RWS.tell . singletonDL

runPureInteract :: PureInteract a -> WordStream -> ([Word8], a)
runPureInteract act = go . RWS.runRWS (unPureInteract act) ()
  where
    go (a, _, dl) = (unDL dl [], a)
