module Graphics.Avatars.Pixelated where

import Codec.Picture (DynamicImage)
import Data.Digest.Pure.MD5 (MD5Digest, md5)
import Data.ByteString.Lazy.Internal (packChars)

-- | A seed to use in generating an avatar. Can be created from a String by
-- using the `createSeed` function.
newtype Seed = Seed { unSeed :: MD5Digest }
  deriving (Show)

-- | A generated avatar.
newtype Avatar = Avatar { unAvatar :: DynamicImage }

-- | Create a seed from a given String.
--
-- >>> createSeed "Hello"
-- Seed {unSeed = 8b1a9953c4611296a827abf8c47804d7}
createSeed :: String -> Seed
createSeed = Seed . md5 . packChars

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
