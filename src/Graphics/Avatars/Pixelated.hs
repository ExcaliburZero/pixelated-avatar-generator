module Graphics.Avatars.Pixelated
(
  Seed(..), Avatar(..), Color(..), AvatarGrid(..), showGrid,

  createSeed, generateAvatar, saveAvatar, colorFromSeed, generateAvatarGrid
)
where

import Codec.Picture (DynamicImage)
import Data.Char (ord)
import Data.ByteString.Lazy.Internal (packChars)
import Data.Digest.Pure.MD5 (md5)
import Data.List.Split (chunksOf)

-------------------------------------------------------------------------------
-- Types

-- | A seed to use in generating an avatar. Can be created from a String by
-- using the `createSeed` function.
newtype Seed = Seed { unSeed :: String }
  deriving (Eq, Show)

-- | A generated avatar.
newtype Avatar = Avatar { unAvatar :: DynamicImage }

-- | A color for an avatar.
data Color = Black | Blue | Green | Grey | Orange | Purple | Red | Yellow
  deriving (Eq, Show)

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

-------------------------------------------------------------------------------
-- Seeds

-- | Creates a seed from a given String.
--
-- >>> createSeed "Hello"
-- Seed {unSeed = "8b1a9953c4611296a827abf8c47804d7"}
createSeed :: String -> Seed
createSeed = Seed . show . md5 . packChars

-------------------------------------------------------------------------------
-- Avatars

-- | Generates an avatar from the given seed.
generateAvatar :: Seed -> Avatar
generateAvatar seed = undefined
 where avatar = undefined
       color = undefined
       grid = undefined

-- | Saves the given avatar to the given file path.
--
-- @
-- makeAvatar :: Seed -> FilePath -> IO ()
-- makeAvatar seed path = do
--   let avatar = generateAvatar seed path
--   saveAvatar avatar path
-- @
saveAvatar :: Avatar -> FilePath -> IO ()
saveAvatar avatar path = undefined

-------------------------------------------------------------------------------
-- Colors

-- | Pick an avatar color using the given seed.
--
-- >>> colorFromSeed $ Seed {unSeed = "8b1a9953c4611296a827abf8c47804d7"}
-- Orange
colorFromSeed :: Seed -> Color
colorFromSeed seed = color avg
  where digits = take 2 $ unSeed seed
        avg = (((ord . head) digits) + ((ord . last) digits)) `div` 2
        color a
          | a < ord '2'  = Black
          | a < ord '4'  = Blue
          | a < ord '6'  = Green
          | a < ord '8'  = Grey
          | a < ord 'a'  = Orange
          | a < ord 'c'  = Purple
          | a < ord 'e'  = Red
          | otherwise    = Yellow

-------------------------------------------------------------------------------
-- AvatarGrids

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
numToGrid s = grid
  where grid = (map . map) convertToPixel $ (map . map) ord numGrid
        numGrid = chunksOf 4 s
        convertToPixel = (> ord '7')
