import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import Control.Applicative

{-
    You can think of functions as boxes, that contain their
    eventual result.

    Applicative law: pure f <*> x = fmap f x
    In conclusion, applicative functors aren't just interesting,
    they're also useful, because they allow us to combine different
    computations, such as I/O computations, non-deterministic computations,
    computations that might have failed, etc. by using the applicative style.
-}

-- Implement this function
{- sequenceA' :: (Applicative f) => [f a] -> f [a] -}

main :: IO ()
main = hspec $ do
    describe "Applicative" $ do
        it "works with IO ()" $ do
            pending
            {- combined <- (___ <$> return "hello" <*> return ___) -}
            {- combined `shouldBe` "helloworld" -}
        it "works with combination of functions" $ do
            pending
            {- ((___) <$> (+3) <*> (*100) $ ___) -}
                {- `shouldBe` 508 -}
        it "works with lambdas" $ do
            pending
            {- ((___) <$> (+3) <*> (*2) <*> (/2) $ 5) -}
                {- `shouldBe` [8.0,10.0,2.5] -}
        it "works with zipList" $ do
            pending
            -- newtype ZipList a = ZipList { getZipList :: [a] }
            {- (___ $ (___) <$> ZipList [1,2,3] <*> ZipList [100,100,100]) -}
                {- `shouldBe` [101,102,103] -}
        it "applies a function between two applicatives" $ do
            pending
            {- fmap (___) (Just 4) `shouldBe` Just [4] -}
            {- liftA2 (___) (___) (Just [4]) `shouldBe` Just [3,4] -}
            {- ((___) <$> ___ <*> Just [4]) `shouldBe` Just [3,4] -}
        it "can create a sequence" $ do
            pending
            {- sequenceA' [Just 3, Just 2, Just 1] -}
                {- `shouldBe` Just [3,2,1] -}
            {- sequenceA' [Just 3, Nothing, Just 1] -}
                {- `shouldBe` Nothing -}
            {- sequenceA' [(+3),(+2),(+1)] 3 -}
                {- `shouldBe` [6,5,4] -}
        it "can tell if a number satisfies all the predicates in a list" $ do
            pending
            {- map (___) [(>4),(<10),odd] -}
                {- `shouldBe` [True,True,True] -}
            {- (___ $ map (\f -> f 7) [(>4),(<10),odd]) -}
                {- `shouldBe` True -}
