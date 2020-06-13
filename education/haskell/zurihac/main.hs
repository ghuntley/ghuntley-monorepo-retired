-- module1

-- x = 200
-- y = x + 2

-- main = putStrLn "Hello World!"

-- module2

-- spell :: Integer -> String
-- spell int = 
--     case int of
--         1 -> "one"
--         2 -> "two"
--         3 -> "three"
--         4 -> "four"
--         5 -> "five"
--         _ -> "I don't know this number!"


-- module3

-- idk :: (Num a, Ord a) => a -> a
-- -- idk :: Num a => a -> a
-- idk x = 
--     if (x < 10) then (negate x) else (x + 10)

preferJ:: Foldable t => t Char -> t Char -> t Char
preferJ x y = if (elem 'j' x) then x else y

idk2 :: (Num a, Ord a) => a -> a
idk2 x =
    case (x < 10) of
        True -> negate x
        False -> x + 10


data Maybe a = Nothing | Just a
