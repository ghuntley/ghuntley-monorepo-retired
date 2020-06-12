import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

type Birds = Int
type Pole = (Birds,Birds)

landLeft1 :: Birds -> Pole -> Pole
landLeft1 n (left, right) = (left+n, right)

landRight1 :: Birds -> Pole -> Pole
landRight1 n (left, right) = (left, right+n)

x -: f = f x

-- Improved logic with checks
landLeft :: Birds -> Pole -> Maybe Pole
landLeft n (left, right)
    | abs ((left + n) - right) < 4 = Just (left + n, right)
    | otherwise                    = Nothing

landRight :: Birds -> Pole -> Maybe Pole
landRight n (left, right)
    | abs (left - (right + n)) < 4 = Just (left, right + n)
    | otherwise                    = Nothing

banana :: Pole -> Maybe Pole
banana _ = Nothing

{-
   Starts from (0,0)
   2 lands left
   2 lands right
   1 lands left
-}
{- routine :: Maybe Pole -}

{-
   Starts from (0,0)
   2 lands left
   Nothing
   2 lands right
   1 lands left
-}
{- routineWithFall :: Maybe Pole -}

-- pattern match with <- for the first character of "hello"
{- justH :: Maybe Char -}


-- same as above, but use Maybe empty string as input
{- wopwop :: Maybe Char -}

main :: IO()
main = hspec $ do
    describe "Bird Landing" $ do
        it "can leverage do notation" $ do
            pending
            {- routine `shouldBe` Just (3,2) -}
        it "will fall when Nothing is used" $ do
            pending
            {- routineWithFall `shouldBe` Nothing -}
        it "can use pattern match at binding" $ do
            pending
            {- justH `shouldBe` Just 'h' -}
        it "returns Nothing when pattern matching fails" $ do
            pending
            {- wopwop `shouldBe` Nothing -}
