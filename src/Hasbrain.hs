{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Hasbrain where

import Control.Applicative ((<|>))
import Control.Monad.RWS.Strict (RWS)
import qualified Control.Monad.RWS.Strict as RWS
import Data.Functor (($>))
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Data.Traversable (for)
import Data.Void
import Data.Word (Word8)
import qualified Text.Megaparsec as MP

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
  | Output
  | Input
  | JumpFromLeft
  | JumpFromRight
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

{-
A brainfuck program is a sequence of instructions and other (ignored)
characters. The only possible syntax error is mismatched brackets.
-}

type Parser = MP.Parsec Void Text

type ParseError = MP.ParseErrorBundle Text Void

-- | Register a delayed parse error for an open bracket
registerOpenBracket :: Parser ()
registerOpenBracket =
  MP.registerFancyFailure $
    S.singleton $
      MP.ErrorFail "unmatched left bracket"

-- | Check if an unmatched open bracket has previously been registered, removing
-- the error from the parser state if so and otherwise throwing a parse error.
registerCloseBracket :: Parser ()
registerCloseBracket = do
  s <- MP.getParserState
  case MP.stateParseErrors s of
    (_ : es) -> MP.updateParserState $
      \s' -> s' {MP.stateParseErrors = es}
    [] -> fail "unmatched right bracket"

pInstr :: Parser Instr
pInstr =
  ">" $> PointerRight
    <|> "<" $> PointerLeft
    <|> "+" $> Increment
    <|> "-" $> Decrement
    <|> "." $> Output
    <|> "," $> Input
    <|> comment
    <|> bracket
  where
    comment = fmap Comment $
      MP.takeWhile1P Nothing $
        \c -> c `notElem` ['>', '<', '+', '-', '.', ',', '[', ']']
    -- at this point in the chain the only possible characters left are the
    -- brackets
    bracket = do
      mb <- MP.lookAhead $ MP.optional "["
      res <- case mb of
        Just _ -> registerOpenBracket $> JumpFromLeft
        Nothing -> registerCloseBracket $> JumpFromRight
      _ <- MP.anySingle
      pure res

pAllInstrs :: Parser [Instr]
pAllInstrs = do
  ins <- MP.many pInstr
  b <- MP.atEnd
  -- somewhat inelegant, but since we use 'many' we need to make sure that
  -- unmatched bracket errors are actually thrown
  if b
    then pure ins
    else pInstr $> error "pAllInstrs - internal error"

-- | Parse a brainfuck program from the given named input
parseProgram ::
  -- | name of the input
  Text ->
  -- | the input
  Text ->
  Either Text [Instr]
parseProgram name =
  either (Left . T.pack . MP.errorBundlePretty) Right
    . MP.runParser pAllInstrs (T.unpack name)

stripComments :: [Instr] -> [Instr]
stripComments = filter $ \x -> case x of
  Comment _ -> False
  _ -> True

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

-- | The instruction tape, with the head of the second list being the current
-- instruction
data InstrTape = InstrTape [Instr] [Instr]
  deriving (Eq, Ord, Show)

-- | The data tape
data DataTape = DataTape WordStream !Word8 WordStream

-- | The data tape starts with all cells initialized to zero
initCellTape :: DataTape
initCellTape = DataTape zeroes 0 zeroes
  where
    zeroes = constStream 0

initInstrTape :: [Instr] -> InstrTape
initInstrTape = InstrTape []

popInstrRight :: InstrTape -> Maybe (Instr, InstrTape)
popInstrRight (InstrTape ls (r : rs)) = Just (r, InstrTape ls rs)
popInstrRight _ = Nothing

popInstrLeft :: InstrTape -> Maybe (Instr, InstrTape)
popInstrLeft (InstrTape (l : ls) rs) = Just (l, InstrTape ls rs)
popInstrLeft _ = Nothing

putInstrRight :: Instr -> InstrTape -> InstrTape
putInstrRight i (InstrTape ls rs) = InstrTape ls (i : rs)

putInstrLeft :: Instr -> InstrTape -> InstrTape
putInstrLeft i (InstrTape ls rs) = InstrTape (i : ls) rs

-- | Move right along the tape until just after an unmatched 'JumpFromRight' is
-- encountered, or until the end of the tape is reached
seekRight :: InstrTape -> InstrTape
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
seekLeft :: InstrTape -> InstrTape
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

data BrainState = BrainState
  { brainInstrs :: InstrTape,
    brainData :: DataTape
  }

initBrainState :: [Instr] -> BrainState
initBrainState is = BrainState (initInstrTape is) initCellTape

stepState :: MonadInteract m => BrainState -> m (Maybe BrainState)
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

-- | Run a 'BrainState' until it halts
runBrain :: MonadInteract m => BrainState -> m BrainState
runBrain b = do
  mb <- stepState b
  case mb of
    Just b' -> runBrain b'
    Nothing -> pure b

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
