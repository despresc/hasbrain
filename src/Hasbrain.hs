{-# LANGUAGE OverloadedStrings #-}

module Hasbrain where

import Control.Applicative ((<|>))
import Data.Functor (($>))
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void
import qualified Text.Megaparsec as MP
import Data.Word (Word8)

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
    <|> comment
    <|> bracket
  where
    -- all the other source characters will have been recognized by now
    comment = fmap Comment $ MP.takeWhile1P Nothing $ \c -> c /= '[' && c /= ']'
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

-- | A tape with focus for the cells and instructions
data Tape a = Tape [a] !a [a]

-- | We start with an infinite tape for the cells, all initialized to zero
initCellTape :: Tape Word8
initCellTape = Tape (repeat 0) 0 (repeat 0)
