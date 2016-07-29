module Main where

import Console.Options
import Control.Concurrent.Async (mapConcurrently)
import Data.Char (isDigit)
import Data.List (nub)
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Data.Version (makeVersion)
import Graphics.Avatars.Pixelated
import System.Random (randomIO)

main :: IO ()
main = defaultMain $ do
  -- Define program information
  programName "pixelated-avatar-generator"
  programDescription "Generates pixelated avatar images"
  programVersion $ makeVersion [0,1,2]

  -- Gather program flags and arguments
  flagS <- flagParam
    (FlagLong "scaling-factor" <> FlagDescription scalingFlagDescription)
    (FlagRequired scaleParser)
  firstFile <- argument "FILEPATH" Right
  tailFiles <- remainingArguments "FILEPATHS"

  -- Perform the functionality of the program
  action $ \toParam -> do
    let scalingFactor = fromMaybe defaultScalingFactor (toParam flagS)
    let paths = nub $ toParam firstFile : toParam tailFiles
    _ <- mapConcurrently (saveRandomAvatar scalingFactor) paths
    putStrLn $ createSuccessMessage paths

-- | The default avatar scaling factor that is used when the user does not
-- specify a custom scaling factor via the "--scaling-factor" flag.
defaultScalingFactor :: Int
defaultScalingFactor = 32

-- | The description of the "--scaling-factor" flag.
scalingFlagDescription :: String
scalingFlagDescription = unwords
  [ "Use a custom scaling factor. The scaling factor is multiplied by 8 to get"
  , "the dimensions of the image. For example, a scaling factor of 4 would"
  , "yield a 32x32px image. The default scaling factor is 32."
  ]

-- | Prints a success message noting the number of images created.
--
-- >>> createSuccessMessage ["1.png", "2.png"]
-- "Successfully created 2 avatars."
createSuccessMessage :: [FilePath] -> String
createSuccessMessage paths = message
  where numberOfAvatars = (show . length) paths
        message = unwords ["Successfully created", numberOfAvatars, "avatars."]

-- | Creates and saves a random avatar at the given filepath using the given
-- scaling factor.
saveRandomAvatar :: Int -> FilePath -> IO ()
saveRandomAvatar factor path = do
  seed <- show <$> (randomIO :: IO Double)
  saveCustomAvatar factor path seed

-- | Creates and saves an avatar at the given filepath using the given seed
-- string and the given scaling factor.
saveCustomAvatar :: Int -> FilePath -> String -> IO ()
saveCustomAvatar factor path string = do
  let seed = createSeed string
  let avatar = generateAvatar seed
  let avatarScaled = scaleAvatar factor avatar
  saveAvatar avatarScaled path

-- | Attempts to parse a scaling factor, which must be a non-positive integer
-- (1, 2, 3, ...).
--
-- >>> scaleParser "1"
-- Right 1
-- >>> scaleParser "0"
-- Left "The given String is not a positive integer"
-- >>> scaleParser "-1"
-- Left "The given String is not a positive integer"
scaleParser :: String -> Either String Int
scaleParser string = if isPosInt string
                     then Right ((read :: String -> Int) string)
                     else Left "The given String is not a positive integer"
 where isPosInt s   = isNaturalNum s && isNotZero s
       isNaturalNum = all isDigit
       isNotZero s  = not $ all (== '0') s
