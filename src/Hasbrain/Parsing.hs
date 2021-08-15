{-# LANGUAGE OverloadedStrings #-}

module Hasbrain.Parsing where

import Control.Applicative ((<|>))
import Data.Functor (($>))
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void
import Hasbrain.Syntax
import qualified Text.Megaparsec as MP

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
