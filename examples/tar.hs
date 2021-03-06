import Development.Shake
import Development.Shake.CLI
import Development.Shake.FilePath

main = shakeWithArgs $ do
    want ["result.tar"]
    "*.tar" *> \out -> do
        contents <- readFileLines $ replaceExtension out "txt"
        need contents
        system' "tar" $ ["-cf",out] ++ contents
