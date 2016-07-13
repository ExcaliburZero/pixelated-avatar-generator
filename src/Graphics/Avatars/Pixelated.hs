-- |
-- Module      : Graphics.Avatars.Pixelated
-- Description : Contains types and functions for generating pixelated avatars.
-- Copyright   : (c) Christopher Wells, 2016
-- License     : MIT
-- Maintainer  : cwellsny@nycap.rr.com
--
-- This module provides types and functions for creating and working with
-- pixelated avatars.
--
-- Avatars can be generated by providing a `Seed`. Seeds can be created by
-- passing a random String into `createSeed`. The given String is then turned
-- into an MD5 checksum, which is used as the seed value.
--
-- Once a Seed has been created, an Avatar can be generated from it by passing
-- it into `generateAvatar`. By default, Avatars start at a size of 8x8px. The
-- size of an Avatar can be increased by passing it into `scaleAvatar`. Once
-- you have scaled the Avatar to the size you want it to be it can then be
-- saved to a file by passing it into `saveAvatar`.
--
-- By default, the `saveAvatar` function saves the avatar as a png image file,
-- however you can also save avatars in other image formats by using
-- `saveAvatarWith`. It allows you to specify the function to use to convert
-- the avatar image into an image ByteString.
--
-- = Example
-- The following is an example showing how to construct a function which will
-- generate a 256x256px avatar from a given seed string, and save it at the
-- given location.
--
-- @
-- import Graphics.Avatars.Pixelated
--
-- createAndSaveAvatar :: String -> FilePath -> IO ()
-- createAndSaveAvatar s path = saveAvatar avatar path
--   where avatar = scaleAvatar 32 $ generateAvatar seed
--         seed   = createSeed s
-- @
module Graphics.Avatars.Pixelated
(
  -- * Types
  -- ** Seed
  Seed(..), createSeed,

  -- ** Avatar
  Avatar(..), generateAvatar, scaleAvatar, saveAvatar, saveAvatarWith, convertAvatarToImage,

  -- ** Color
  Color(..), getColorValue, colorFromSeed,
  
  -- ** Avatar Grid
  AvatarGrid(..), showGrid, generateAvatarGrid,

  -- ** Utility
  scaleList
)
where

import Codec.Picture (encodePng, generateImage, Image(..), PixelRGB8(..))
import Data.Char (ord)
import qualified Data.ByteString.Lazy as B (ByteString, writeFile)
import Data.ByteString.Lazy.Internal (packChars)
import Data.Digest.Pure.MD5 (md5)
import Data.List.Split (chunksOf)

-------------------------------------------------------------------------------
-- Seeds

-- | A seed to use in generating an avatar. Can be created from a String by
-- using the `createSeed` function.
--
-- Seeds are expected to be 32 character hexidecimal MD5 checksums.
newtype Seed = Seed { unSeed :: String }
  deriving (Eq, Show)

-- | Creates a seed from a given String.
--
-- >>> createSeed "Hello"
-- Seed {unSeed = "8b1a9953c4611296a827abf8c47804d7"}
createSeed :: String -> Seed
createSeed = Seed . show . md5 . packChars

-------------------------------------------------------------------------------
-- Avatars

-- | A generated avatar.
data Avatar = Avatar {
      color :: Color
    , grid  :: AvatarGrid
  }
  deriving (Eq)

instance Show Avatar where
  show a = (show . color) a ++ "\n" ++ ((show . grid) a)

-- | Generates an avatar from the given seed.
generateAvatar :: Seed -> Avatar
generateAvatar seed = avatar
 where avatar = Avatar {
             color = aColor
           , grid = aGrid
         }
       aColor = colorFromSeed seed
       aGrid = generateAvatarGrid seed

-- | Scales the given Avatar by the given scaling factor.
scaleAvatar :: Int -> Avatar -> Avatar
scaleAvatar factor avatar = avatar { grid = AvatarGrid scaledGrid }
  where scaledGrid = ((scaleList factor) . (map (scaleList factor))) unscaledGrid
        unscaledGrid = unAvatarGrid $ grid avatar

-- | Saves the given avatar as a png image file to the given file path.
--
-- @
-- makeAvatar :: Seed -> FilePath -> IO ()
-- makeAvatar seed path = do
--   let avatar = generateAvatar seed path
--   saveAvatar avatar path
-- @
saveAvatar :: Avatar -> FilePath -> IO ()
saveAvatar = saveAvatarWith encodePng

-- | Saves the given avatar to the given file location, using the given
-- function to encode it into a specific image format.
--
-- Some examples of encoding functions are `Codec.Picture.Tiff.encodeTiff` and
-- `Codec.Picture.Png.encodePng`.
--
-- @
-- saveTiffAvatar :: Seed -> FilePath -> IO ()
-- saveTiffAvatar seed path = do
--   let avatar = generateAvatar seed path
--   saveAvatarWith encodeTiff avatar path
-- @
saveAvatarWith :: (Image PixelRGB8 -> B.ByteString) -> Avatar -> FilePath -> IO ()
saveAvatarWith conversion avatar path = do
  let image = conversion $ convertAvatarToImage avatar
  B.writeFile path image

-- | Converts the given Avatar into an Image.
convertAvatarToImage :: Avatar -> Image PixelRGB8
convertAvatarToImage avatar = image
  where image = generateImage getPixel dimension dimension
        dimension = length colorGrid
        getPixel x y = colorGrid !! y !! x
        colorGrid = (map . map) (toPixel $ color avatar) $ unAvatarGrid $ grid avatar
        toPixel c v = if v then getColorValue c else PixelRGB8 255 255 255

-------------------------------------------------------------------------------
-- Colors

-- | A color for an avatar.
data Color = Black | Blue | Green | Grey | Orange | Purple | Red | Yellow
  deriving (Eq, Show, Enum)

-- | Converts the given color into a RGB pixel representation.
getColorValue :: Color -> PixelRGB8
getColorValue c
  | c == Black  = PixelRGB8 0   0   0
  | c == Blue   = PixelRGB8 0   0   200
  | c == Green  = PixelRGB8 0   200 0
  | c == Grey   = PixelRGB8 150 150 150
  | c == Orange = PixelRGB8 255 140 65
  | c == Purple = PixelRGB8 130 0   130
  | c == Red    = PixelRGB8 200 0   0
  | otherwise   = PixelRGB8 230 230 0

-- | Picks an avatar color using the given seed.
--
-- >>> colorFromSeed $ Seed {unSeed = "8b1a9953c4611296a827abf8c47804d7"}
-- Grey
colorFromSeed :: Seed -> Color
colorFromSeed = genColor . dSum . unSeed
  where twoDigits n = map ord $ take 2 n
        dSum n = foldr (+) 1 $ twoDigits n
        genColor a = [Black .. Yellow] !! (a `mod` 8)

-------------------------------------------------------------------------------
-- AvatarGrids

-- | A grid of boolean values representing an Avatar. True values indicate
-- colored pixels, and False values indicate blank pixels.
newtype AvatarGrid = AvatarGrid { unAvatarGrid :: [[Bool]] }
  deriving (Eq)

-- | Converts the grid into a String representation.
instance Show AvatarGrid where
  show x = (showGrid . unAvatarGrid) x

-- | The left half of an AvatarGrid.
newtype AvatarGridSide = AvatarGridSide { unAvatarGridSide :: [[Bool]] }

-- | Converts the grid side into a String representation.
instance Show AvatarGridSide where
  show x = (showGrid . unAvatarGridSide) x

-- | Converts a grid of boolean values into a String representation.
--
-- >>> putStrLn $ showGrid [[True, False], [False, True]]
-- █ 
--  █
showGrid :: [[Bool]] -> String
showGrid g = (init . unlines) $ (map . map) showPixel g

-- | Converts a boolean value into a character representation for a pixel.
--
-- >>> showPixel True
-- '\9608'
-- >>> showPixel False
-- ' '
showPixel :: Bool -> Char
showPixel p = if p then '█' else ' '

-- | Generates an AvatarGrid using the given Seed.
--
-- It works by generating the left half of the grid using the contents of the
-- Seed, and then mirroring the left half to create the full grid.
--
-- >>> generateAvatarGrid Seed {unSeed = "8b1a9953c4611296a827abf8c47804d7"}
-- ██ ██ ██
-- ██    ██
-- █      █
--   █  █  
-- ██    ██
-- ████████
-- █  ██  █
--   █  █  
generateAvatarGrid :: Seed -> AvatarGrid
generateAvatarGrid = mirrorGrid . generateAvatarGridSide

-- | Creates a full AvatarGrid by mirroring the given AvatarGridSide on the
-- y-axis.
mirrorGrid :: AvatarGridSide -> AvatarGrid
mirrorGrid side = AvatarGrid $ map mirror $ unAvatarGridSide side
  where mirror l = l ++ (reverse l)

-- | Generates the right side of an AvatarGrid using the given seed.
generateAvatarGridSide :: Seed -> AvatarGridSide
generateAvatarGridSide = AvatarGridSide . numToGrid . unSeed

-- | Converts the given hexidecimal number String into a grid of boolean values.
numToGrid :: String -> [[Bool]]
numToGrid s = boolGrid
  where boolGrid = (map . map) convertToPixel $ (map . map) ord numGrid
        numGrid  = chunksOf 4 s
        convertToPixel = (> ord '7')

-------------------------------------------------------------------------------
-- Utilities

-- | Scales the given list by the given scaling factor.
--
-- >>> scaleList 2 [1, 2]
-- [1,1,2,2]
-- >>> scaleList 3 [0, 1]
-- [0,0,0,1,1,1]
scaleList :: Int -> [a] -> [a]
scaleList _     []     = []
scaleList factor (x:xs) = replicate factor x ++ scaleList factor xs
