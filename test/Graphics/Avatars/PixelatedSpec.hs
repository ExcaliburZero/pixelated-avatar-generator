module Graphics.Avatars.PixelatedSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck

import Codec.Picture
import Data.String (IsString(..))
import qualified Data.ByteString.Lazy as B (ByteString(..), readFile)
import System.IO (hGetContents)
import System.IO.Unsafe (unsafePerformIO)

import Graphics.Avatars.Pixelated

-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec

spec :: Spec
spec = do

  -----------------------------------------------------------------------------
  -- Seeds

  describe "createSeed" $ do
    it "creates a seed using an md5 checksum" $ do
      createSeed "" `shouldBe` Seed {unSeed = "d41d8cd98f00b204e9800998ecf8427e"}

  -----------------------------------------------------------------------------
  -- Avatars

  describe "Avatar" $ do
    it "can be compared for equality" $ do
      helloAvatar == helloAvatar `shouldBe` True
      helloAvatar /= testAvatar  `shouldBe` True
    it "can be converted into a string representation" $ do
      show helloAvatar `shouldBe` "Grey\n" ++ helloAvatarGridString

  describe "generateAvatar" $ do
    it "generates an avatar from a seed" $ do
      generateAvatar helloSeed `shouldBe` helloAvatar

  describe "scaleAvatar" $ do
    it "scales an avatar by a given factor" $ do
      scaleAvatar 2 testAvatar `shouldBe` testAvatar2x

  describe "convertAvatarToImage" $ do
    it "converts an avatar into an image" $ do
      encodePng (convertAvatarToImage helloAvatar) `shouldBe` helloAvatarImagePng

  ----------------------------------------
  -- Image Conversion

  describe "encodeToPng" $ do
    it "can encode an image into png format" $ do
      (encodeToPng . convertAvatarToImage) helloAvatar `shouldBe` helloAvatarImagePng

  describe "encodeToGif" $ do
    it "can encode an image into gif format" $ do
      (encodeToGif . convertAvatarToImage) helloAvatar `shouldBe` helloAvatarImageGif

  describe "encodeToTiff" $ do
    it "can encode an image into tiff format" $ do
      (encodeToTiff . convertAvatarToImage) helloAvatar `shouldBe` helloAvatarImageTiff

  -----------------------------------------------------------------------------
  -- Colors

  describe "colorFromSeed" $ do
    it "picks a color based on the given seed" $ do
      colorFromSeed (helloSeed) `shouldBe` Grey
    it "can choose Black" $ do
      colorFromSeed (Seed {unSeed = "c4000000000000000000000000000000"}) `shouldBe` Black
    it "can choose Blue" $ do
      colorFromSeed (Seed {unSeed = "ec000000000000000000000000000000"}) `shouldBe` Blue
    it "can choose Green" $ do
      colorFromSeed (Seed {unSeed = "a8000000000000000000000000000000"}) `shouldBe` Green
    it "can choose Grey" $ do
      colorFromSeed (Seed {unSeed = "aa000000000000000000000000000000"}) `shouldBe` Grey
    it "can choose Orange" $ do
      colorFromSeed (Seed {unSeed = "c8000000000000000000000000000000"}) `shouldBe` Orange
    it "can choose Purple" $ do
      colorFromSeed (Seed {unSeed = "c9000000000000000000000000000000"}) `shouldBe` Purple
    it "can choose Red" $ do
      colorFromSeed (Seed {unSeed = "c2000000000000000000000000000000"}) `shouldBe` Red
    it "can choose Yellow" $ do
      colorFromSeed (Seed {unSeed = "8f000000000000000000000000000000"}) `shouldBe` Yellow

  describe "getColorValue" $ do
    it "returns a pixel representation of a color" $ do
      getColorValue Orange `shouldBe` PixelRGB8 255 140 65
    it "can process Black" $ do
      getColorValue Black `shouldBe` PixelRGB8 41 41 41
    it "can process Blue" $ do
      getColorValue Blue `shouldBe` PixelRGB8 104 182 255
    it "can process Green" $ do
      getColorValue Green `shouldBe` PixelRGB8 114 220 131
    it "can process Grey" $ do
      getColorValue Grey `shouldBe` PixelRGB8 150 150 150
    it "can process Orange" $ do
      getColorValue Orange `shouldBe` PixelRGB8 255 140 65
    it "can process Purple" $ do
      getColorValue Purple `shouldBe` PixelRGB8 208 148 255
    it "can process Red" $ do
      getColorValue Red `shouldBe` PixelRGB8 255 87 87
    it "can process Yellow" $ do
      getColorValue Yellow `shouldBe` PixelRGB8 255 231 148

  -----------------------------------------------------------------------------
  -- Avatar Grids

  describe "generateAvatarGrid" $ do
    it "creates an avatar grid from a seed" $ do
      generateAvatarGrid helloSeed `shouldBe` helloAvatarGrid

  describe "AvatarGrid" $ do
    it "can be represented as a String" $ do
      show helloAvatarGrid `shouldBe` helloAvatarGridString

  -----------------------------------------------------------------------------
  -- Utilities

  describe "scaleList" $ do
   it "scales a list by a given factor" $ do
     scaleList 3 [0, 1] `shouldBe` [0, 0, 0, 1, 1, 1]
   it "can scale a list by a factor of one" $ do
     scaleList 1 [0, 1] `shouldBe` [0, 1]

-------------------------------------------------------------------------------
-- Values

helloSeed :: Seed
helloSeed = Seed {unSeed = "8b1a9953c4611296a827abf8c47804d7"}

helloAvatar :: Avatar
helloAvatar = Avatar {
      color = Grey
    , grid  = helloAvatarGrid
  }

helloAvatarGrid :: AvatarGrid
helloAvatarGrid = AvatarGrid ([
      [True, True, False, True, True, False, True, True]
    , [True, True, False, False, False, False, True, True]
    , [True, False, False, False, False, False, False, True]
    , [False, False, True, False, False, True, False, False]
    , [True, True, False, False, False, False, True, True]
    , [True, True, True, True, True, True, True, True]
    , [True, False, False, True, True, False, False, True]
    , [False, False, True, False, False, True, False, False]
  ])

helloAvatarGridString :: String
helloAvatarGridString = (init . unlines) [
    "██ ██ ██"
  , "██    ██"
  , "█      █"
  , "  █  █  "
  , "██    ██"
  , "████████"
  , "█  ██  █"
  , "  █  █  "
  ]

helloAvatarImagePng :: B.ByteString
helloAvatarImagePng = unsafePerformIO $ B.readFile "test/Graphics/Avatars/helloAvatar.png"

helloAvatarImageGif :: B.ByteString
helloAvatarImageGif = unsafePerformIO $ B.readFile "test/Graphics/Avatars/helloAvatar.gif"

helloAvatarImageTiff :: B.ByteString
helloAvatarImageTiff = unsafePerformIO $ B.readFile "test/Graphics/Avatars/helloAvatar.tiff"

testAvatar :: Avatar
testAvatar = Avatar {
      color = Orange
    , grid  = AvatarGrid [
         [True, False]
       , [False, True]
      ]
  }

testAvatar2x :: Avatar
testAvatar2x = Avatar {
      color = Orange
    , grid  = AvatarGrid [
         [True, True, False, False]
       , [True, True, False, False]
       , [False, False, True, True]
       , [False, False, True, True]
      ]
  }

-------------------------------------------------------------------------------
-- Utility Functions

unRight :: Either a b -> b
unRight x = case x of
  Right y -> y
  Left _  -> error "Unexpected Left"
