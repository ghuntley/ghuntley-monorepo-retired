import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

{- Shape type should have Circle (3 args) and Rectangle (4 args) data constructors -}
{- data Shape ... -}

{- Create the Point type, with a Point data constructor that uses 2 args -}
{- Use the Point for Circle and Rectangle -}

{- surface :: Shape -> Float -}

{- nudge :: Shape -> Float -> Float -> Shape -}

{- baseCircle :: Float -> Shape -}

{- baseRectangle :: Float -> Float -> Shape -}

main :: IO()
main = hspec $ do
    describe "Custom Types" $ do
        it "can calculate surface for two types" $ do
            pending
            {- (surface $ Circle 10 20 10) `shouldBe` 314.15927 -}
            {- (surface $ Rectangle 0 0 100 100) -}
                {- `shouldBe` 10000.0 -}
            {- (surface $ Rectangle (-10) 0 100 100) -}
                {- `shouldBe` 11000.0 -}
            {- (surface $ Rectangle 0 0 (-100) (-100)) -}
                {- `shouldBe` 10000.0 -}
        it "can print out the Shape by adding Show" $ do
            pending
            {- Circle 10 20 5 `shouldBe` Circle 10.0 20.0 5.0 -}
        it "can use Value constructors as functions" $ do
            -- Use map function here
            pending
            {- ___ ___ [4,5] -}
                {- `shouldBe` [Circle 10.0 20.0 4.0, Circle 10.0 20.0 5.0] -}
        it "can calculate surface for two types with Points" $ do
            pending
            {- (surface $ Circle (Point 10 20) 10) `shouldBe` 314.15927 -}
            {- (surface $ Rectangle (Point 0 0) (Point 100 100)) -}
                {- `shouldBe` 10000.0 -}
        it "can nudge a Shape, still holding its dimension" $ do
            pending
           {- (nudge (Circle (Point 0 0) 3) 2 4) -}
                {- `shouldBe` Circle (Point 2.0 4.0) 3.0 -}
           {- (nudge (Rectangle (Point 0 0) (Point 4 4)) 2 4) -}
                {- `shouldBe` Rectangle (Point 2.0 4.0) (Point 6.0 8.0) -}
        it "can nudge a Shape starting with base shapes" $ do
            pending
           {- (nudge (baseCircle 3) 2 4) -}
                {- `shouldBe` Circle (Point 2.0 4.0) 3.0 -}
           {- (nudge (baseRectangle 4 4)) 2 4 -}
                {- `shouldBe` Rectangle (Point 2.0 4.0) (Point 6.0 8.0) -}
