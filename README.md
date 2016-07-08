# Pixelated Avatar Generator [![Travis CI Status](https://api.travis-ci.org/ExcaliburZero/pixelated-avatar-generator.svg)](https://travis-ci.org/ExcaliburZero/pixelated-avatar-generator) [![Coverage Status](https://coveralls.io/repos/github/ExcaliburZero/pixelated-avatar-generator/badge.svg?branch=master)](https://coveralls.io/github/ExcaliburZero/pixelated-avatar-generator?branch=master)
Pixelated Avatar Generator is a Haskell library and application for generating pixelated avatar images from seed values.

```haskell
import Graphics.Avatars.Pixelated

createAndSaveAvatar :: String -> FilePath -> IO ()
createAndSaveAvatar s path = saveAvatar avatar path
  where avatar = scaleAvatar 32 $ generateAvatar seed
        seed   = createSeed s
```

## Executable
An example executable program that uses the library is also provided. It creates an avatar from a given seed string and saves the created `.png` image to a given file location.

The executable can be compiled by running the following command:

```
$ stack build
```

The executable can then by run by running it with `stack exec` and providing it the desired filepath of the output file including the `.png` extension and a random seed string.

```
$ stack exec pixelated-avatar-generator image.png "Hello, World"
Creating avatar at image.png
Green
█ ████ █
        
  ████  
█  ██  █
████████
█ █  █ █
█      █
████████
Successfully created avatar, and saved it to image.png
```

### Usage
```
Usage: pixelated-avatar-generator FILEPATH SEEDSTRING

FILEPATH   -- The location to save the generated avatar at. "img/test.png"
SEEDSTRING -- The string to use to generate the avatar. "Hello"
```

## License
The source code of Pixelated Avatar Generator is available under the [MIT license](https://opensource.org/licenses/MIT), see `LICENSE` for more information.
