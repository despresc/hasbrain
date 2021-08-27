{-# LANGUAGE OverloadedStrings #-}

module Hasbrain.Interactive where

import Data.Char (isHexDigit)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Void (Void)
import Data.Word (Word8)
import Hasbrain.Loop
  ( parseProgram,
    pureEvalInstrs,
  )
import qualified Options.Applicative as Opt
import qualified Text.Megaparsec as MP
import qualified Text.Megaparsec.Char as MPC

type Parser a = MP.Parsec Void Text a

type ParseError = MP.ParseError Text Void

-- | Parse an entire input stream of (possibly space-separated) bytes
-- encoded as pairs of hexadecimal numbers
hexBytes :: Parser [Word8]
hexBytes = do
  MPC.space
  hs <- MP.many $ oneByte <* MPC.space
  MP.eof
  pure hs
  where
    hex = do
      c <- MP.satisfy isHexDigit MP.<?> "hex digit"
      let go x
            | x <= 57 = x - 48
            | otherwise = x - 87
      pure $ toEnum $ go $ fromEnum c
    oneByte = MP.label "byte" $ do
      d1 <- hex
      d2 <- hex
      pure $ 16 * d1 + d2

-- | Parse the brainfuck program input
parseInput :: InputSource -> InputFormat -> Text -> Either Text [Word8]
parseInput inps inpf =
  either (Left . T.pack . MP.errorBundlePretty) Right
    . MP.parse p inputName
  where
    p = case inpf of
      InputHexadecimal -> hexBytes
    inputName = case inps of
      InputFromFile f -> f
      InputFromStdin -> "<stdin>"

parseInputIO :: InputSource -> InputFormat -> Text -> IO [Word8]
parseInputIO inps inpf t = case parseInput inps inpf t of
  Left x -> error $ "Invalid input:\n" <> T.unpack x
  Right a -> pure a

-- | Show the output of a brainfuck program
showOutput :: OutputFormat -> [Word8] -> Text
showOutput OutputHexadecimal = T.pack . concatMap go
  where
    showHex n
      | n <= 9 = toEnum $ fromEnum n + 48
      | otherwise = toEnum $ fromEnum n + 87
    go x = [showHex d1, showHex d2]
      where
        (d1, d2) = x `divMod` 16
showOutput OutputAsciiEscaped = T.pack . fmap (toEnum . fromEnum)

-- | The possible formats for the input of the interpreted brainfuck program
data InputFormat
  = InputHexadecimal
  deriving (Eq, Ord, Show)

-- | The source of the brainfuck program input
data InputSource
  = InputFromFile FilePath
  | InputFromStdin
  deriving (Eq, Ord, Show)

-- | Where the brainfuck program output should be written
data OutputTarget
  = OutputToFile FilePath
  | OutputToStdout
  deriving (Eq, Ord, Show)

-- | The possible formats of the brainfuck program output
data OutputFormat
  = OutputHexadecimal
  | OutputAsciiEscaped
  deriving (Eq, Ord, Show)

-- | The command-line options for the hasbrain executable. These are mostly
-- exclusive to the "interpret" command, but that is the only command we have at
-- the moment.
data InterpretOptions = InterpretOptions
  { programFile :: FilePath,
    inputFormat :: InputFormat,
    inputSource :: InputSource,
    outputTarget :: OutputTarget,
    outputFormat :: OutputFormat
  }
  deriving (Eq, Ord, Show)

data CommandOptions
  = CommandInterpret InterpretOptions
  deriving (Eq, Ord, Show)

parseInterpretOptions :: Opt.Mod Opt.CommandFields CommandOptions
parseInterpretOptions =
  Opt.command
    "interpret"
    (Opt.info pInterpret $ Opt.progDesc "Run the interpreter on the program in FILE")
  where
    pInterpret =
      fmap CommandInterpret $
        InterpretOptions
          <$> Opt.argument Opt.str optProgramFile
          <*> Opt.option pInputFormat optInputFormat
          <*> Opt.option pInputSource optInputSource
          <*> Opt.option pOutputTarget optOutputTarget
          <*> Opt.option pOutputFormat optOutputFormat

    pInputFormat = do
      s <- Opt.str
      case (s :: Text) of
        "hex" -> pure InputHexadecimal
        _ -> Opt.readerError "Recognized formats: hex"
    optInputFormat =
      Opt.long "in-format"
        <> Opt.help "format of the brainfuck program input"
        <> Opt.showDefaultWith (const "hex")
        <> Opt.value InputHexadecimal

    optProgramFile =
      Opt.metavar "FILE"

    pInputSource = InputFromFile <$> Opt.str
    optInputSource =
      Opt.long "in-source"
        <> Opt.short 'f'
        <> Opt.help "file from which the brainfuck program will read its input"
        <> Opt.showDefaultWith (const "<stdin>")
        <> Opt.value InputFromStdin

    pOutputTarget = OutputToFile <$> Opt.str
    optOutputTarget =
      Opt.long "out-target"
        <> Opt.short 'o'
        <> Opt.help "file to which the brainfuck program will write its output"
        <> Opt.showDefaultWith (const "<stdout>")
        <> Opt.value OutputToStdout

    pOutputFormat = do
      s <- Opt.str
      case (s :: Text) of
        "hex" -> pure OutputHexadecimal
        "ascii-escaped" -> pure OutputAsciiEscaped
        _ -> Opt.readerError "Recognized formats: hex, ascii-escaped"
    optOutputFormat =
      Opt.long "out-format"
        <> Opt.help "format of the brainfuck program output"
        <> Opt.showDefaultWith (const "hex")
        <> Opt.value OutputHexadecimal

parseCommandOptions :: Opt.Parser CommandOptions
parseCommandOptions = Opt.subparser parseInterpretOptions

infoCommandOptions :: Opt.ParserInfo CommandOptions
infoCommandOptions =
  Opt.info (parseCommandOptions Opt.<**> Opt.helper) $
    Opt.fullDesc
      <> Opt.header "hasbrain - the Haskell brainfuck tool suite"

runHasbrain :: IO ()
runHasbrain = do
  commandOptions <- Opt.execParser infoCommandOptions
  case commandOptions of
    CommandInterpret options -> do
      progText <- T.readFile $ programFile options
      program <- case parseProgram (programFile options) progText of
        Left x -> error $ "Brainfuck syntax error:\n" <> T.unpack x
        Right a -> pure a
      inputText <- case inputSource options of
        InputFromFile f -> T.readFile f
        InputFromStdin -> T.getContents
      input <- parseInputIO (inputSource options) (inputFormat options) inputText
      let writeOutputText = case outputTarget options of
            OutputToFile f -> T.writeFile f
            OutputToStdout -> T.putStrLn . ("\n" <>)
      let writeOutput = writeOutputText . showOutput (outputFormat options)
      let output = pureEvalInstrs program input
      writeOutput output
