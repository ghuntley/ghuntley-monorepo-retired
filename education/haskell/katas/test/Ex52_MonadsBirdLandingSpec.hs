module Ex52_MonadsBirdLandingSpec
  ( spec
  ) where

import Test.Hspec

main :: IO ()
main = hspec spec

type Birds = Int

type Pole = (Birds, Birds)

{-
   The rules for bird landing: the person keeps his balance if the
   number of birds on the left side and the number of birds on the
   right side of the pole is within three.
-}

-- landLeft1 :: Birds -> Pole -> Pole
-- landRight1 :: Birds -> Pole -> Pole

-- Improved logic with checks
-- landLeft :: Birds -> Pole -> Maybe Pole
-- landRight :: Birds -> Pole -> Maybe Pole
-- banana :: Pole -> Maybe Pole

landLeft1 :: Birds -> Pole -> Pole
landLeft1 n (l,r) = (l+n, r)

landRight1 :: Birds -> Pole -> Pole
landRight1 n (l,r) = (l,r+n)

-- Improved logic with checks
landLeft :: Birds -> Pole -> Maybe Pole
landLeft n (l,r)
  | abs((l+n) - r) < 4 = Just (l+n, r)
  | otherwise = Nothing


landRight :: Birds -> Pole -> Maybe Pole
landRight n (l,r)
  | abs((r+n) - l) < 4 = Just (l,r+n)
  | otherwise = Nothing

banana :: Pole -> Maybe Pole
banana _ = Nothing

x -: f = f x

spec :: Spec
spec = do
    describe "Bird Landing" $ do
        it "lets a bird land on the left" $ do

             landLeft1 2 (0,0) `shouldBe` (2,0)
             landRight1 1 (1,2) `shouldBe` (1,3)
             landRight1 (-1) (1,2) `shouldBe` (1,1)
        it "can chain landLeft and landRight" $ do

             (landLeft1 2 (landRight1 1 (landLeft1 1 (0, 0))))
                 `shouldBe` (3,1)
        it "can apply functions by first writing the param and the function" $ do

             100 -: (*3) `shouldBe` 300
             True -: not `shouldBe` False
             (0,0) -: landLeft1 2 `shouldBe` (2,0)
        it "can repeatedly land birds on the pole in a more readable manner" $ do

             (0,0) -: landLeft1 1 -: landRight1 1 -: landLeft1 2
                 `shouldBe` (3,1)
        it "can protect itself from too many birds on one side" $ do

             (landLeft 2 (0,0)) `shouldBe` Just (2,0)
             (landLeft 10 (0,3)) `shouldBe` Nothing
        it "can use Monad's binding to talk to Maybe values" $ do

             (landRight 1 (0,0) >>= landLeft 2) `shouldBe` Just (2,1)
             (Nothing >>= landLeft 2) `shouldBe` Nothing
        it "can be chained to simulate birdy landings" $ do

             (return (0,0) >>= landRight 2 >>= landLeft 2 >>= landRight 2)
                 `shouldBe` (Just (2,4))
             (return (0,0) >>= landLeft 1 >>= landRight 4 >>= landLeft (-1) >>= landRight (-2))
                 `shouldBe` Nothing
        it "can simulate a fall by stepping on a banana skin" $ do

             (return (0,0) >>= landLeft 1 >>= banana >>= landRight 1)
                 `shouldBe` Nothing
        it "can use the >> fn to return a nothing" $ do

             (return (0,0) >>= landLeft 1 >> Nothing >>= landRight 1)
                 `shouldBe` Nothing
