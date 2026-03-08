module Simulation where

import Data.List (sortBy)
import Data.Ord (comparing)

-- Cube states/moves are represented as permutations of the 48 non-centre
-- stickers; algorithms are compositions of permutations.
--
-- The solved state is the identity permutation: [0, 1, 2, ..., 47].
--
-- The order is like in blind solving: 0-7 is the U face, starting at UBL and
-- continuing clockwise to UB, UBR, UL, etc., then 8-15 is the L face, then F,
-- R, B, D.
--
--          ┌U───────┐
--          │ 0  1  2│
--          │ 7     3│
--          │ 6  5  4│
-- ┌L───────┼F───────┼R───────┬B───────┐
-- │ 8  9 10│16 17 18│24 25 26│32 33 34│
-- │15    11│23    19│31    27│39    35│
-- │14 13 12│22 21 20│30 29 28│38 37 36│
-- └────────┼D───────┼────────┴────────┘
--          │40 41 42│
--          │47    43│
--          │46 45 44│
--          └────────┘

type Perm = [Int]

identity :: Perm
identity = [0..47]

compose :: Perm -> Perm -> Perm
compose p q = map (q !!) p

twice :: Perm -> Perm
twice p = compose p p

inverse :: Perm -> Perm
inverse p = map snd $ sortBy (comparing fst) (zip p identity)

cycle4 :: Int -> Int -> Int -> Int -> Perm
cycle4 a b c d = map replace identity
  where replace i
          | i == a    = b
          | i == b    = c
          | i == c    = d
          | i == d    = a
          | otherwise = i

moveU :: Perm
moveU = foldr1 compose
  [ cycle4 0 2 4 6     -- UBL-UBR-UFR-UFL (U face corner stickers)
  , cycle4 1 3 5 7     -- UB-UR-UF-UL (U face edge stickers)
  , cycle4 34 26 18 10 -- BUL-RUB-FUR-LUF
  , cycle4 33 25 17 9  -- BU-RU-FU-LU
  , cycle4 32 24 16 8  -- BUR-RUF-FUL-LUB
  ]

moveD :: Perm
moveD = foldr1 compose
  [ cycle4 40 42 44 46
  , cycle4 41 43 45 47
  , cycle4 22 30 38 14
  , cycle4 21 29 37 13
  , cycle4 20 28 36 12
  ]

moveF :: Perm
moveF = foldr1 compose
  [ cycle4 16 18 20 22
  , cycle4 17 19 21 23
  , cycle4 6 24 42 12
  , cycle4 5 31 41 11
  , cycle4 4 30 40 10
  ]

moveB :: Perm
moveB = foldr1 compose
  [ cycle4 32 34 36 38
  , cycle4 33 35 37 39
  , cycle4 2 8 46 28
  , cycle4 1 15 45 27
  , cycle4 0 14 44 26
  ]

moveR :: Perm
moveR = foldr1 compose
  [ cycle4 24 26 28 30
  , cycle4 25 27 29 31
  , cycle4 4 32 44 20
  , cycle4 3 39 43 19
  , cycle4 2 38 42 18
  ]

moveL :: Perm
moveL = foldr1 compose
  [ cycle4 8 10 12 14
  , cycle4 9 11 13 15
  , cycle4 0 16 40 36
  , cycle4 7 23 47 35
  , cycle4 6 22 46 34
  ]
