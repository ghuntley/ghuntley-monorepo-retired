{-# LANGUAGE UnicodeSyntax #-}

module Ex24_TypesMakingTypesSpec
  ( spec
  ) where

import Test.Hspec
import Test.QuickCheck
import Data.List.Unicode
import Prelude.Unicode
import Data.Monoid.Unicode

main :: IO ()
main = hspec spec

{- Shape type should have Circle (3 args) and Rectangle (4 args) data constructors -}
data Shape' = Circle' Float Float Float | Rectangle' Float Float Float Float deriving (Show, Eq)

{- Create the Point type, with a Point data constructor that uses 2 args -}
data Point = Point Float Float deriving (Show, Eq)
data Shape = Circle Point Float | Rectangle Point Point deriving (Show, Eq)

surface :: Shape -> Float
surface (Circle _ r) = π ⋅ r ⋅ r
surface (Rectangle (Point x1 y1) (Point x2 y2)) = (abs $ x2 - x1) * (abs $ y2 - y1)

nudge :: Shape -> Float -> Float -> Shape
nudge (Circle (Point x y) r) a b = Circle (Point (x+a) (y+b)) r
nudge (Rectangle (Point x1 y1) (Point x2 y2)) a b = Rectangle (Point (x1+a) (y1+b)) (Point (x2+a) (y2+b))

baseCircle :: Float -> Shape
baseCircle r = (Circle (Point 0.0 0.0) r)

baseRectangle :: Float -> Float -> Shape
baseRectangle w h = (Rectangle (Point 0.0 0.0) (Point w h))

spec :: Spec
spec = do
    describe "Custom Types" $ do
        it "can calculate surface for two types" $ do
             (surface $ Circle (Point 10 20) 10) `shouldBe` 314.15927
             (surface $ Rectangle (Point 0 0) (Point 100 100))
                 `shouldBe` 10000.0
             (surface $ Rectangle (Point (-10) 0) (Point 100 100))
                 `shouldBe` 11000.0
             (surface $ Rectangle (Point 0 0) (Point (-100) (-100)))
                 `shouldBe` 10000.0
        it "can print out the Shape by adding Show" $ do
             Circle' 10 20 5 `shouldBe` Circle' 10.0 20.0 5.0
        it "can use Value constructors as functions" $ do
            -- Use map function here
             map (Circle' 10 20) [4,5]
                 `shouldBe` [Circle' 10.0 20.0 4.0, Circle' 10.0 20.0 5.0]
        it "can calculate surface for two types with Points" $ do
             (surface $ Circle (Point 10 20) 10) `shouldBe` 314.15927
             (surface $ Rectangle (Point 0 0) (Point 100 100))
                 `shouldBe` 10000.0
        it "can nudge a Shape, still holding its dimension" $ do
            (nudge (Circle (Point 0 0) 3) 2 4)
                 `shouldBe` Circle (Point 2.0 4.0) 3.0
            (nudge (Rectangle (Point 0 0) (Point 4 4)) 2 4)
                 `shouldBe` Rectangle (Point 2.0 4.0) (Point 6.0 8.0)
        it "can nudge a Shape starting with base shapes" $ do
            (nudge (baseCircle 3) 2 4)
                 `shouldBe` Circle (Point 2.0 4.0) 3.0
            (nudge (baseRectangle 4 4)) 2 4
                 `shouldBe` Rectangle (Point 2.0 4.0) (Point 6.0 8.0)
