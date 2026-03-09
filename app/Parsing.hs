module Parsing where

import Simulation
import Data.Char (toLower)
import Data.Maybe (fromMaybe)

type FaceMap = [(Char, Perm)]
data Modifier = None | Two | Prime

startingFaceMap :: FaceMap
startingFaceMap =
  [ ('U', moveU)
  , ('D', moveD)
  , ('F', moveF)
  , ('B', moveB)
  , ('R', moveR)
  , ('L', moveL)
  ]

rotFaceMap :: Char -> Modifier -> FaceMap -> FaceMap
rotFaceMap axis None fm = map (\(c, _) -> (c, lookupFm (src axis c))) fm
  where
    lookupFm c = fromMaybe identity $ lookup c fm
    src 'x' 'U' = 'F'; src 'x' 'F' = 'D'; src 'x' 'D' = 'B'; src 'x' 'B' = 'U'
    src 'y' 'F' = 'R'; src 'y' 'R' = 'B'; src 'y' 'B' = 'L'; src 'y' 'L' = 'F'
    src 'z' 'U' = 'L'; src 'z' 'L' = 'D'; src 'z' 'D' = 'R'; src 'z' 'R' = 'U'
    src _ c = c
rotFaceMap axis Two   fm = rotFaceMap axis None $ rotFaceMap axis None fm
rotFaceMap axis Prime fm = rotFaceMap axis None $ rotFaceMap axis Two  fm

expandSlice :: Char -> Modifier -> String
expandSlice c None = case c of
  'M' -> "x' R L'"
  'E' -> "y' U D'"
  'S' -> "z F' B"
  'u' -> "y D"
  'd' -> "y' U"
  'f' -> "z B"
  'b' -> "z' F"
  'r' -> "x L"
  'l' -> "x' R"
  _ -> pure c
expandSlice c Two   = expandSlice c None ++ expandSlice c None
expandSlice c Prime = expandSlice c None ++ expandSlice c Two

parseModifier :: String -> (Modifier, String)
parseModifier ('2':'\'':cs)           = (Two,   cs)
parseModifier ('2':cs)                = (Two,   cs)
parseModifier (c:cs) | c `elem` "\'3" = (Prime, cs)
parseModifier cs                      = (None,  cs)

caseForAlg :: String -> Perm
caseForAlg = aux startingFaceMap
  where
    aux :: FaceMap -> String -> Perm
    aux _ [] = identity

    -- Slice/wide moves
    aux fm (c:cs) | c `elem` "MESudfbrl" =
      let
        (mod_, rest) = parseModifier cs
        expanded = expandSlice c mod_
      in aux fm (expanded ++ rest)

    aux fm (c:'w':cs) | c `elem` "UDFBRL" =
      let
        (mod_, rest) = parseModifier cs
        expanded = expandSlice (toLower c) mod_
      in aux fm (expanded ++ rest)

    -- Rotations
    aux fm (c:cs) | c `elem` "xyzXYZ" =
      let
        (mod_, rest) = parseModifier cs
        fm' = rotFaceMap (toLower c) mod_ fm
      in aux fm' rest

    -- Face turns
    aux fm (c:cs) | c `elem` "UDFBRL" =
      let
        (mod_, rest) = parseModifier cs
        move = fromMaybe identity $ lookup c fm
        move' = case mod_ of Two   -> twice move
                             Prime -> inverse move
                             None  -> move
      in compose move' $ aux fm rest

    -- Skip irrelevant characters
    aux fm (_:cs) = aux fm cs
