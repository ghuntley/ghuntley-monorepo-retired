{-
    Getting string from the input, transforming it with a function
    and then outputting is so common, that `interact` was added.
    interact :: (String -> String) -> IO ()
-}

main = interact shortLinesOnly

{-
    It can be further simplified with one line
-}
