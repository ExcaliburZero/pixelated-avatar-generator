# Pixelated Avatar Generator [![Travis CI Status](https://api.travis-ci.org/ExcaliburZero/pixelated-avatar-generator.svg)](https://travis-ci.org/ExcaliburZero/pixelated-avatar-generator) [![Coverage Status](https://coveralls.io/repos/github/ExcaliburZero/pixelated-avatar-generator/badge.svg?branch=master)](https://coveralls.io/github/ExcaliburZero/pixelated-avatar-generator?branch=master)
Pixelated Avatar Generator is a Haskell library and application for generating pixelated avatar images from seed values.

```haskell
import Graphics.Avatars.Pixelated

createAndSaveAvatar :: String -> FilePath -> IO ()
createAndSaveAvatar s path = saveAvatar avatar path
  where avatar = scaleAvatar 32 $ generateAvatar seed
        seed   = createSeed s
```

## License
The source code of pixelated Avatar Generator is available under the [MIT license](https://opensource.org/licenses/MIT), see `LICENSE` for more information.
