module Main where

import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure, exitSuccess)
import System.IO (hPutStrLn, stderr)
import Text.Read (readMaybe)
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
    -- Help text
    go (h:_) _ _ | h `elem` ["-h", "--help"] = usage >> exitSuccess
    -- SVG scale factor
    go (s:v:rest) cfg alg | s `elem` ["-s", "--scale"] =
      case (readMaybe v :: Maybe Int) of
        Nothing -> do
          hPutStrLn stderr "Scale factor must be an integer"
          exitFailure
        Just i -> go rest (cfg { cfgScale = i }) alg
    -- Face colours
    go (s:v:rest) cfg alg | s `elem` ["-u", "--up"]    = go rest (cfg { cfgUp    = v }) alg
    go (s:v:rest) cfg alg | s `elem` ["-f", "--front"] = go rest (cfg { cfgFront = v }) alg
    go (s:v:rest) cfg alg | s `elem` ["-b", "--back"]  = go rest (cfg { cfgBack  = v }) alg
    go (s:v:rest) cfg alg | s `elem` ["-l", "--left"]  = go rest (cfg { cfgLeft  = v }) alg
    go (s:v:rest) cfg alg | s `elem` ["-r", "--right"] = go rest (cfg { cfgRight = v }) alg
    go (s:v:rest) cfg alg | s `elem` ["-d", "--down"]  = go rest (cfg { cfgDown  = v }) alg
    -- Alg string (positional)
    go (a:rest) cfg Nothing  = go rest cfg (Just a)
    -- No arguments expected after alg
    go (a:_) _ (Just _) = do
      hPutStrLn stderr $ "Unexpected argument: " ++ a
      usage >> exitFailure

usage :: IO ()
usage = do
  prog <- getProgName
  putStr $ unlines
    [ "Usage: " ++ prog ++ " [OPTIONS] \"<algorithm>\""
    , ""
    , "Options:"
    , "  -h, --help            Show this message"
    , "  -s, --scale N         SVG canvas scale factor (integer, default: " ++ show (cfgScale defaultConfig) ++ ")"
    , "  -u, --up    COLOUR    U face colour (default: " ++ cfgUp    defaultConfig ++ ")"
    , "  -f, --front COLOUR    F face colour (default: " ++ cfgFront defaultConfig ++ ")"
    , "  -b, --back  COLOUR    B face colour (default: " ++ cfgBack  defaultConfig ++ ")"
    , "  -l, --left  COLOUR    L face colour (default: " ++ cfgLeft  defaultConfig ++ ")"
    , "  -r, --right COLOUR    R face colour (default: " ++ cfgRight defaultConfig ++ ")"
    , "  -d, --down  COLOUR    D face colour (default: " ++ cfgDown  defaultConfig ++ ")"
    , ""
    , "COLOUR may be any SVG colour name or hex value (e.g. 'limegreen', '#ff0000')."
    ]

main :: IO ()
main = do
  args <- getArgs
  (cfg, alg) <- parseArgs args
  putStrLn $ renderSvg cfg (caseForAlg alg)
