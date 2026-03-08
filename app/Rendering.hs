module Rendering where

import Simulation (Perm)

data Config = Config
  { cfgSize  :: Int    -- SVG canvas size (px)
  , cfgUp    :: String -- U face colour
  , cfgFront :: String -- F face colour
  , cfgBack  :: String -- B face colour
  , cfgLeft  :: String -- L face colour
  , cfgRight :: String -- R face colour
  , cfgDown  :: String -- D face colour
  } deriving (Show)

data XmlTag = Standard    String [(String, String)] [XmlTag]
            | SelfClosing String [(String, String)]

-- --------------
-- XML primitives
-- --------------

instance Show XmlTag where
  show = showIndented 0

showIndented :: Int -> XmlTag -> String
showIndented depth (SelfClosing name attrs) =
  indent depth ++ "<" ++ name ++ showAttrs attrs ++ "/>"
showIndented depth (Standard name attrs []) =
  indent depth ++ "<" ++ name ++ showAttrs attrs ++ "></" ++ name ++ ">"
showIndented depth (Standard name attrs children) =
  indent depth ++ "<" ++ name ++ showAttrs attrs ++ ">\n"
  ++ unlines (map (showIndented (depth + 1)) children)
  ++ indent depth ++ "</" ++ name ++ ">"

indent :: Int -> String
indent depth = replicate (depth * 2) ' '

showAttrs :: [(String, String)] -> String
showAttrs [] = ""
showAttrs attrs = " " ++ unwords (map showAttr attrs)
  where showAttr (k, v) = k ++ "=\"" ++ v ++ "\""

-- -------------------------
-- Config-dependent geometry
-- -------------------------

gap :: Config -> Int
gap cfg = cfgSize cfg `div` 42

stickerWidth :: Config -> Int
stickerWidth cfg = 10 * gap cfg

stickerDepth :: Config -> Int
stickerDepth cfg = 3 * gap cfg

-- -----------
-- SVG helpers
-- -----------

drawSticker :: Int -> Int -> Int -> Int -> String -> XmlTag
drawSticker width height x y fill =
  SelfClosing "rect"
    [ ("stroke",       "black")
    , ("stroke-width", "1")
    , ("width",        show width)
    , ("height",       show height)
    , ("x",            show x)
    , ("y",            show y)
    , ("fill",         fill)
    ]

-- x or y pixel offset for sticker at grid index 0/1/2
stickerPos :: Config -> Int -> Int
stickerPos cfg index = 2 * gap cfg + stickerDepth cfg + index * (stickerWidth cfg + gap cfg)

drawUpSticker :: Config -> Int -> Int -> String -> XmlTag
drawUpSticker cfg xi yi =
  drawSticker (stickerWidth cfg) (stickerWidth cfg) (stickerPos cfg xi) (stickerPos cfg yi)

drawBackSticker :: Config -> Int -> String -> XmlTag
drawBackSticker cfg xi =
  drawSticker (stickerWidth cfg) (stickerDepth cfg) (stickerPos cfg xi) (gap cfg)

drawRightSticker :: Config -> Int -> String -> XmlTag
drawRightSticker cfg yi =
  drawSticker (stickerDepth cfg) (stickerWidth cfg)
              (cfgSize cfg - gap cfg - stickerDepth cfg) (stickerPos cfg yi)

drawFrontSticker :: Config -> Int -> String -> XmlTag
drawFrontSticker cfg xi =
  drawSticker (stickerWidth cfg) (stickerDepth cfg)
              (stickerPos cfg xi) (cfgSize cfg - gap cfg - stickerDepth cfg)

drawLeftSticker :: Config -> Int -> String -> XmlTag
drawLeftSticker cfg yi =
  drawSticker (stickerDepth cfg) (stickerWidth cfg) (gap cfg) (stickerPos cfg yi)

colourAt :: Config -> Perm -> Int -> String
colourAt cfg perm pos =
  let i = perm !! pos
  in if      i <=  7 then cfgUp    cfg
     else if i <= 15 then cfgLeft  cfg
     else if i <= 23 then cfgFront cfg
     else if i <= 31 then cfgRight cfg
     else if i <= 39 then cfgBack  cfg
     else                 cfgDown  cfg

-- ------------
-- SVG renderer
-- ------------

renderSvg :: Config -> Perm -> String
renderSvg cfg perm = show $
  Standard "svg"
    [ ("xmlns",  "http://www.w3.org/2000/svg")
    , ("width",  show (cfgSize cfg))
    , ("height", show (cfgSize cfg))
    ]
    (upStickers ++ backStickers ++ rightStickers ++ frontStickers ++ leftStickers)
  where
    c = colourAt cfg perm

    upStickers =
      [ drawUpSticker cfg 0 0 (c 0)       -- UBL
      , drawUpSticker cfg 1 0 (c 1)       -- UB
      , drawUpSticker cfg 2 0 (c 2)       -- UBR
      , drawUpSticker cfg 0 1 (c 7)       -- UL
      , drawUpSticker cfg 1 1 (cfgUp cfg) -- centre
      , drawUpSticker cfg 2 1 (c 3)       -- UR
      , drawUpSticker cfg 0 2 (c 6)       -- UFL
      , drawUpSticker cfg 1 2 (c 5)       -- UF
      , drawUpSticker cfg 2 2 (c 4)       -- UFR
      ]

    backStickers =
      [ drawBackSticker cfg 0 (c 34)
      , drawBackSticker cfg 1 (c 33)
      , drawBackSticker cfg 2 (c 32)
      ]

    rightStickers =
      [ drawRightSticker cfg 0 (c 26)
      , drawRightSticker cfg 1 (c 25)
      , drawRightSticker cfg 2 (c 24)
      ]

    frontStickers =
      [ drawFrontSticker cfg 0 (c 16)
      , drawFrontSticker cfg 1 (c 17)
      , drawFrontSticker cfg 2 (c 18)
      ]

    leftStickers =
      [ drawLeftSticker cfg 0 (c 8)
      , drawLeftSticker cfg 1 (c 9)
      , drawLeftSticker cfg 2 (c 10)
      ]
