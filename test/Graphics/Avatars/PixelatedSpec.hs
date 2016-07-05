module Graphics.Avatars.PixelatedSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck

import Graphics.Avatars.Pixelated

-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "createSeed" $ do
    it "creates a seed using an md5 checksum" $ do
      createSeed "" `shouldBe` Seed {unSeed = "d41d8cd98f00b204e9800998ecf8427e"}
  describe "colorFromSeed" $ do
    it "picks a color based on the given seed" $ do
      colorFromSeed (Seed {unSeed = "d41d8cd98f00b204e9800998ecf8427e"}) `shouldBe` Orange
    it "can choose Black" $ do
      colorFromSeed (Seed {unSeed = "01000000000000000000000000000000"}) `shouldBe` Black
    it "can choose Blue" $ do
      colorFromSeed (Seed {unSeed = "23000000000000000000000000000000"}) `shouldBe` Blue
    it "can choose Green" $ do
      colorFromSeed (Seed {unSeed = "45000000000000000000000000000000"}) `shouldBe` Green
    it "can choose Grey" $ do
      colorFromSeed (Seed {unSeed = "67000000000000000000000000000000"}) `shouldBe` Grey
    it "can choose Orange" $ do
      colorFromSeed (Seed {unSeed = "89000000000000000000000000000000"}) `shouldBe` Orange
    it "can choose Purple" $ do
      colorFromSeed (Seed {unSeed = "ab000000000000000000000000000000"}) `shouldBe` Purple
    it "can choose Red" $ do
      colorFromSeed (Seed {unSeed = "cd000000000000000000000000000000"}) `shouldBe` Red
    it "can choose Yellow" $ do
      colorFromSeed (Seed {unSeed = "ef000000000000000000000000000000"}) `shouldBe` Yellow
  describe "generateAvatarGrid" $ do
    it "creates an avatar grid from a seed" $ do
      generateAvatarGrid (Seed {unSeed = "8b1a9953c4611296a827abf8c47804d7"})
        `shouldBe` helloAvatarGrid
  describe "AvatarGrid" $ do
    it "can be represented as a String" $ do
      show helloAvatarGrid `shouldBe` helloAvatarGridString

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
