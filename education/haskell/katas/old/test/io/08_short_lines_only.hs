{-
    Print only those lines where the length is < 10
    Try it with: cat test/io/haiku.txt | runhaskell test/io/08_short_lines_only.hs
-}

main = do
    contents <- getContents
    putStr (shortLinesOnly contents)

{- shortLinesOnly :: String -> String -}
