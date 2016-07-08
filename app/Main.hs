module Main where

import Graphics.Avatars.Pixelated
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  checkForProperArgs args
  let path = head args
  let seedString = args !! 1
  putStrLn $ "Creating avatar at " ++ path
  createAndSaveAvatar path seedString
  putStrLn $ "Successfully created avatar, and saved it to " ++ path

checkForProperArgs :: [String] -> IO ()
checkForProperArgs args = if length args /= 2
                          then error $ "Improper number of arguments.\n\n" ++ usageInfo
                          else return ()

usageInfo :: String
usageInfo = unlines [
     "Usage: pixelated-avatar-generator FILEPATH SEEDSTRING"
   , ""
   , "FILEPATH   -- The location to save the generated avatar at. \"img/test.png\""
   , "SEEDSTRING -- The string to use to generate the avatar. \"Hello\""
  ]

createAndSaveAvatar :: FilePath -> String -> IO ()
createAndSaveAvatar path s = do
  let seed = createSeed s
  let avatar = generateAvatar seed
  print avatar
  let avatarScaled = scaleAvatar 32 avatar
  saveAvatar avatarScaled path
