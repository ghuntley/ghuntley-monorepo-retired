{-
    IO is a Functor
    instance Functor IO where
        fmap f action = do
            result <- action
            return (f result)
-}

main = do
    putStrLn "Write something:"
    line <-  .... -- make the input backwards
    putStrLn $ "You said " ++ line ++ " backwards!"
    putStrLn $ "Yes, you really said " ++ line ++ " backwards!"
