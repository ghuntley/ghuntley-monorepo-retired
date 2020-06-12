{-
    withFile' :: FilePath -> IOMode -> (Handle -> IO a) -> IO a
-}
import System.IO

withFile' :: FilePath -> IOMode -> (Handle -> IO a) -> IO a
withFile' ___

main = do
    withFile' "test/io/girlfriend.txt" ReadMode (\handle -> do
        contents <- hGetContents handle
        putStr contents)
