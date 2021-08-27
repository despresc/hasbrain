{-# LANGUAGE OverloadedStrings #-}

module Hasbrain.Loop.Parsing where

import Control.Applicative ((<|>))
import Data.Functor (($>))
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Hasbrain.Loop.Instructions
import qualified Text.Megaparsec as MP

{-
A brainfuck program is a sequence of instructions and other (ignored)
characters. The only possible syntax error is mismatched brackets, the reporting
of which is handled through delayed parse errors. Note that this (slightly
hacked-together) scheme only works as long as the only delayed parse error ever
used is 'UnmatchedLeftBracket'!

Parsing is temporarily duplicated here.
-}

type Parser = MP.Parsec BracketError Text

type ParseError = MP.ParseErrorBundle Text BracketError

data BracketError
  = UnmatchedLeftBracket
  | UnmatchedRightBracket
  deriving (Eq, Ord, Show)

instance MP.ShowErrorComponent BracketError where
  showErrorComponent UnmatchedLeftBracket = "unmatched left bracket"
  showErrorComponent UnmatchedRightBracket = "unmatched right bracket"

registerCustomFailure :: MP.MonadParsec e s m => e -> m ()
registerCustomFailure = MP.registerFancyFailure . S.singleton . MP.ErrorCustom

-- | Register a delayed parse error for an open bracket
registerOpenBracket :: Parser ()
registerOpenBracket = registerCustomFailure UnmatchedLeftBracket

-- | Discard a single error from the delayed parse error stack.
discardErrorCustom :: Parser ()
discardErrorCustom = do
  s <- MP.getParserState
  case MP.stateParseErrors s of
    (_ : es) -> do
      MP.updateParserState $ \s' -> s' {MP.stateParseErrors = es}
      pure ()
    [] -> pure ()

pInstr :: Parser Instr
pInstr =
  ">" $> PointerRight
    <|> "<" $> PointerLeft
    <|> addition
    <|> "." $> Output
    <|> "," $> Input
    <|> comment
    <|> loop
  where
    comment = fmap Comment $
      MP.takeWhile1P Nothing $
        \c -> c `notElem` ['>', '<', '+', '-', '.', ',', '[', ']']
    addition = do
      x <- "-" <|> "+" -- lazy way of getting better errors
      t <- MP.takeWhileP Nothing $ \c -> c == '-' || c == '+'
      let (mins, pluss) = T.partition (== '-') $ x <> t
      -- I believe fromIntegral is meant to wrap properly?
      pure $ Add $ fromIntegral $ T.length pluss - T.length mins
    -- at this point in the chain the only possible characters left are the
    -- brackets. the error handling here is slightly delicate.
    loop = do
      mb <- MP.lookAhead $ MP.optional "["
      case mb of
        Nothing -> MP.empty
        Just _ -> do
          registerOpenBracket
          _ <- MP.anySingle
          instrs <- MP.many pInstr
          _ <- MP.optional $ "]" >> discardErrorCustom
          pure $ Loop instrs

pAllInstrs :: Parser [Instr]
pAllInstrs = do
  ins <- MP.many pInstr
  b <- MP.atEnd
  -- somewhat inelegant, but since we use 'many' we need to make sure that
  -- unmatched bracket errors are actually thrown
  if b
    then pure ins
    else MP.customFailure UnmatchedRightBracket

-- | Parse a brainfuck program from the given named input
parseProgram ::
  -- | name of the input
  String ->
  -- | the input
  Text ->
  Either Text [Instr]
parseProgram name =
  either (Left . T.pack . MP.errorBundlePretty) Right
    . MP.runParser pAllInstrs name
