module Main where

import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure)
import Parsing (caseForAlg)
import Rendering (Config(..), renderSvg)

defaultConfig :: Config
defaultConfig = Config
  { cfgSize  = 210
  , cfgUp    = "yellow"
  , cfgFront = "royalblue"
  , cfgBack  = "limegreen"
  , cfgLeft  = "orange"
  , cfgRight = "crimson"
  , cfgDown  = "white"
  }

parseArgs :: [String] -> IO (Config, String)
parseArgs rawArgs = go rawArgs defaultConfig Nothing
  where
    go [] cfg (Just alg) = return (cfg, alg)
    go [] _   Nothing    = usage >> exitFailure
    -- SVG size
    go ("--size":v:rest) cfg alg = go rest (cfg { cfgSize = read v }) alg
    go ("-s"    :v:rest) cfg alg = go rest (cfg { cfgSize = read v }) alg
    -- Face colours
    go ("-u":v:rest) cfg alg = go rest (cfg { cfgUp    = v }) alg
    go ("-f":v:rest) cfg alg = go rest (cfg { cfgFront = v }) alg
    go ("-b":v:rest) cfg alg = go rest (cfg { cfgBack  = v }) alg
    go ("-l":v:rest) cfg alg = go rest (cfg { cfgLeft  = v }) alg
    go ("-r":v:rest) cfg alg = go rest (cfg { cfgRight = v }) alg
    go ("-d":v:rest) cfg alg = go rest (cfg { cfgDown  = v }) alg
    -- Alg string (positional)
    go (a:rest) cfg Nothing  = go rest cfg (Just a)
    go (a:_)    _   (Just _) = do
      putStrLn $ "Unexpected argument: " ++ a
      usage >> exitFailure

usage :: IO ()
usage = do
  prog <- getProgName
  putStr $ unlines
    [ " Usage: " ++ prog ++ " [OPTIONS] \"<algorithm>\""
    , ""
    , " Options:"
    , "   -s, --size N   SVG canvas size in pixels (default: 210)"
    , "   -u COLOUR      U face colour (default: yellow)"
    , "   -f COLOUR      F face colour (default: royalblue)"
    , "   -b COLOUR      B face colour (default: limegreen)"
    , "   -l COLOUR      L face colour (default: orange)"
    , "   -r COLOUR      R face colour (default: crimson)"
    , "   -d COLOUR      D face colour (default: white)"
    , ""
    , " COLOUR may be any SVG colour name or hex value (e.g. 'limegreen', '#ff0000')."
    ]

main :: IO ()
main = do
  args <- getArgs
  (cfg, alg) <- parseArgs args
  putStrLn $ renderSvg cfg (caseForAlg alg)
