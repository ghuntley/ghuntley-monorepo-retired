{-
    Copies one file into the other
    B.readFile will read the content
    B.writeFile will write the content
-}

import System.Environment
import qualified Data.ByteString.Lazy as B

main = do
    (fileName1:fileName2:_) <- getArgs
    copyFile fileName1 fileName2

{- copyFile :: FilePath -> FilePath -> IO () -}
_____
