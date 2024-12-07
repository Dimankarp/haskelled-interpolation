module Main (main) where

import Options.Applicative
import Lib as L


-- Parser for the command-line options
optionsParser :: Parser L.Options
optionsParser =
  Options
    <$> switch
      (long "linear" <> short 'l' <> help "Use linear method")
    <*> switch
      (long "lagrange" <> short 'j' <> help "Use lagrange method")
    <*> option
      auto
      (long "step" <> short 's' <> metavar "FLOAT" <> help "Step size" <> value 1.0 <> showDefault)

-- Main function to parse the options and use them
main :: IO ()
main = do
  opts <- execParser optsParserInfo
  program opts
  where
    optsParserInfo =
      info
        (optionsParser <**> helper)
        (fullDesc <> progDesc "Choose method and step size" <> header "Interpolation program")
