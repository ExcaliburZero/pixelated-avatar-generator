module Graphics.Avatars.Pixelated where

import Codec.Picture (DynamicImage)
import Data.Char (ord)
import Data.Digest.Pure.MD5 (md5)
import Data.ByteString.Lazy.Internal (packChars)

-- | A seed to use in generating an avatar. Can be created from a String by
-- using the `createSeed` function.
newtype Seed = Seed { unSeed :: String }
  deriving (Eq, Show)

-- | A generated avatar.
newtype Avatar = Avatar { unAvatar :: DynamicImage }

-- | A color for an avatar.
data Color = Black | Blue | Green | Grey | Orange | Purple | Red | Yellow
  deriving (Eq, Show)

-- | Create a seed from a given String.
--
-- >>> createSeed "Hello"
-- Seed {unSeed = "8b1a9953c4611296a827abf8c47804d7"}
createSeed :: String -> Seed
createSeed = Seed . show . md5 . packChars

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

-- | Generates a random avatar from the given seed.
generateAvatar :: Seed -> Avatar
generateAvatar seed = undefined

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
