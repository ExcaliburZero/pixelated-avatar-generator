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
      show (createSeed "") `shouldBe` "Seed {unSeed = d41d8cd98f00b204e9800998ecf8427e}"
