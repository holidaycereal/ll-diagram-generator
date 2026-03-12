module Main where

import Options.Applicative
import Parsing (caseForAlg)
import Rendering (Config(..), renderSvg)

defaultConfig :: Config
defaultConfig = Config
  { cfgScale = 1
  , cfgUp    = "#ffff00"
  , cfgFront = "#1447e6"
  , cfgBack  = "#31c950"
  , cfgLeft  = "#ff8c00"
  , cfgRight = "#c11007"
  , cfgDown  = "#ffffff"
  }

main :: IO ()
main = do
  (cfg, alg) <- execParser opts
  putStrLn $ renderSvg cfg (caseForAlg alg)

opts :: ParserInfo (Config, String)
opts =
  info (parser <**> helper)
    ( briefDesc
   <> progDesc "Generate an SVG diagram of the case solved by an algorithm"
    )

parser :: Parser (Config, String)
parser =
  (,)
    <$> configParser
    <*> argument str (metavar "ALGORITHM")

configParser :: Parser Config
configParser =
  Config
    <$> scaleOpt
    <*> colourOpt "up"    'u' cfgUp
    <*> colourOpt "front" 'f' cfgFront
    <*> colourOpt "back"  'b' cfgBack
    <*> colourOpt "left"  'l' cfgLeft
    <*> colourOpt "right" 'r' cfgRight
    <*> colourOpt "down"  'd' cfgDown

scaleOpt :: Parser Int
scaleOpt =
  option auto
    ( long "scale"
   <> short 's'
   <> metavar "INT"
   <> value (cfgScale defaultConfig)
   <> help "SVG canvas scale factor"
    )

colourOpt :: String -> Char -> (Config -> String) -> Parser String
colourOpt longName shortName field =
  strOption
    ( long longName
   <> short shortName
   <> metavar "COLOUR"
   <> value (field defaultConfig)
   <> help ("Colour for the " ++ longName ++ " face")
    )
