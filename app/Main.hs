module Main where

import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure)
import Parsing (caseForAlg)
import Rendering (Config(..), renderSvg)

defaultConfig :: Config
defaultConfig = Config
  { cfgScale = 1
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
    -- SVG scale factor
    go ("--scale":v:rest) cfg alg = go rest (cfg { cfgScale = read v }) alg
    go ("-s"     :v:rest) cfg alg = go rest (cfg { cfgScale = read v }) alg
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
    , "   -s, --scale N  SVG canvas scale factor (default: " ++ show (cfgScale defaultConfig) ++ ")"
    , "   -u COLOUR      U face colour (default: " ++ cfgUp    defaultConfig ++ ")"
    , "   -f COLOUR      F face colour (default: " ++ cfgFront defaultConfig ++ ")"
    , "   -b COLOUR      B face colour (default: " ++ cfgBack  defaultConfig ++ ")"
    , "   -l COLOUR      L face colour (default: " ++ cfgLeft  defaultConfig ++ ")"
    , "   -r COLOUR      R face colour (default: " ++ cfgRight defaultConfig ++ ")"
    , "   -d COLOUR      D face colour (default: " ++ cfgDown  defaultConfig ++ ")"
    , ""
    , " COLOUR may be any SVG colour name or hex value (e.g. 'limegreen', '#ff0000')."
    ]

main :: IO ()
main = do
  args <- getArgs
  (cfg, alg) <- parseArgs args
  putStrLn $ renderSvg cfg (caseForAlg alg)
