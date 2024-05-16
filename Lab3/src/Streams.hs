{-# OPTIONS_GHC -Wall #-}
import FunctorsMonads
import Streams hiding (main)
import Test.Hspec
import Test.Hspec.QuickCheck
-- Раскомментируйте QuickCheck или Hegdehog, в зависимости от того, что будете использовать
-- Документация https://hspec.github.io/quickcheck.html
-- Документация в https://github.com/parsonsmatt/hspec-hedgehog#readme
-- import Test.Hspec.Hedgehog
main :: IO ()
main = hspec $ do
    describe "Functor' Maybe instance" $ do
        it "fmap for Maybe" $ do
            ((+1) <$$> Just 1) `shouldBe` (Just 2 :: Maybe Int)
            ((+1) <$$> Nothing) `shouldBe` (Nothing :: Maybe Int)
    
    describe "Applicative' Maybe instance" $ do
        it "pure' for Maybe" $ do
            (pure' 1 :: Maybe Int) `shouldBe` Just 1
        
        it "<**> for Maybe" $ do
            (Just (+1) <**> Just 1) `shouldBe` (Just 2 :: Maybe Int)
            (Nothing <**> Just 1) `shouldBe` (Nothing :: Maybe Int)
            (Just (+1) <**> Nothing) `shouldBe` (Nothing :: Maybe Int)
    
    describe "Monad' Maybe instance" $ do
        it ">>== for Maybe" $ do
            (Just 1 >>== (\x -> Just (x + 1))) `shouldBe` (Just 2 :: Maybe Int)
            (Nothing >>== (\x -> Just (x + 1))) `shouldBe` (Nothing :: Maybe Int)
        
        it "join' for Maybe" $ do
            join' (Just (Just 1)) `shouldBe` (Just 1 :: Maybe Int)
            join' (Just Nothing) `shouldBe` (Nothing :: Maybe Int)
            join' (Nothing :: Maybe (Maybe Int)) `shouldBe` Nothing

    describe "liftA2'" $ do
        it "lifts a binary function to actions" $ do
            liftA2' (+) (Just 1) (Just 2) `shouldBe` (Just 3 :: Maybe Int)
            liftA2' (+) Nothing (Just 2) `shouldBe` (Nothing :: Maybe Int)
            liftA2' (+) (Just 1) Nothing `shouldBe` (Nothing :: Maybe Int)
    
    describe "seqA" $ do
        it "sequences actions in a list" $ do
            seqA [Just 1, Just 2] `shouldBe` (Just [1, 2] :: Maybe [Int])
            seqA [Just 1, Just 2, Nothing] `shouldBe` (Nothing :: Maybe [Int])

    describe "traverseA" $ do
        it "traverses a list and applies the function to each element" $ do
            traverseA Just [1, 2] `shouldBe` (Just [1, 2] :: Maybe [Int])
            traverseA (\x -> if x > 2 then Just x else Nothing) [1, 3] `shouldBe` (Nothing :: Maybe [Int])
    
    describe "filterA" $ do
        it "filters a list using a predicate with effects" $ do
            filterA (\x -> Just (x > 0)) [-1, 2, 3] `shouldBe` (Just [2, 3] :: Maybe [Int])
            filterA (\x -> if x < 0 then Nothing else Just (x > 1)) [-1, 2, 3] `shouldBe` (Nothing :: Maybe [Int])

    describe "composeM" $ do
        it "composes monadic functions" $ do
            composeM Just Just 1 `shouldBe` (Just 1 :: Maybe Int)
            composeM Just (const Nothing) 1 `shouldBe` (Nothing :: Maybe Int)

    describe "Functor' Either instance" $ do
        it "fmap for Either" $ do
            ((+1) <$$> Right 1) `shouldBe` (Right 2 :: Either String Int)
            ((+1) <$$> (Left "error" :: Either String Int)) `shouldBe` Left "error"

    describe "Applicative' Either instance" $ do
        it "pure' for Either" $ do
            (pure' 1 :: Either String Int) `shouldBe` Right 1
        
        it "<**> for Either" $ do
            (Right (+1) <**> Right 1) `shouldBe` (Right 2 :: Either String Int)
            (Left "error" <**> Right 1) `shouldBe` (Left "error" :: Either String Int)
            (Right (+1) <**> Left "error") `shouldBe` (Left "error" :: Either String Int)

    describe "Monad' Either instance" $ do
        it ">>== for Either" $ do
            (Right 1 >>== (\x -> Right (x + 1))) `shouldBe` (Right 2 :: Either String Int)
            (Left "error" >>== (\x -> Right (x + 1))) `shouldBe` (Left "error" :: Either String Int)
        
        it "join' for Either" $ do
            join' (Right (Right 1)) `shouldBe` (Right 1 :: Either String Int)
            join' (Right (Left "error")) `shouldBe` (Left "error" :: Either String Int)
            join' (Left "error" :: Either String (Either String Int)) `shouldBe` Left "error"

    describe "Functor' ((->) t) instance" $ do
        it "fmap for ((->) t)" $ do
            ((+1) <$$> (*2) $ 5) `shouldBe` 11

    describe "Applicative' ((->) t) instance" $ do
        it "pure' for ((->) t)" $ do
            pure' 1 5 `shouldBe` 1
        
        it "<**> for ((->) t)" $ do
            ((+) <$$> (*2) <**> (+1)) 5 `shouldBe` 16

    describe "Monad' ((->) t) instance" $ do
        it ">>== for ((->) t)" $ do
            ((*2) >>== \x y -> x + y) 3 `shouldBe` 9
            ((+7) >>== \x y -> x + y) 3 `shouldBe` 13

    describe "Streams" $ do
            describe "streamToList" $ do
                it "converts a stream to an infinite list" $ do
                    take 10 (streamToList (sRepeat 1)) `shouldBe` replicate 10 1
                    take 10 (streamToList nats) `shouldBe` [0..9]

            describe "sTake" $ do
                it "takes first n elements from a stream" $ do
                    sTake 5 (sRepeat 1) `shouldBe` replicate 5 1
                    sTake 10 nats `shouldBe` [0..9]

            describe "sRepeat" $ do
                it "creates a stream of repeating elements" $ do
                    sTake 5 (sRepeat 'a') `shouldBe` replicate 5 'a'

            describe "sCycle" $ do
                it "creates a stream by cycling a list" $ do
                    sTake 10 (sCycle [1, 2, 3]) `shouldBe` [1, 2, 3, 1, 2, 3, 1, 2, 3, 1]

            describe "sIterate" $ do
                it "creates a stream by iterating a function" $ do
                    sTake 5 (sIterate (+1) 0) `shouldBe` [0, 1, 2, 3, 4]

            describe "sInterleave" $ do
                it "interleaves two streams" $ do
                    sTake 10 (sInterleave (sRepeat 1) (sRepeat 2)) `shouldBe` [1, 2, 1, 2, 1, 2, 1, 2, 1, 2]

            describe "nats" $ do
                it "is a stream of natural numbers" $ do
                    sTake 10 nats `shouldBe` [0..9]

            describe "ruler" $ do
                it "is a stream representing the ruler function" $ do
                    sTake 10 ruler `shouldBe` [0, 1, 0, 2, 0, 1, 0, 3, 0, 1]

            describe "minMax" $ do
                it "finds the minimum and maximum of a list" $ do
                    minMax [1, 2, 3, 4, 5] `shouldBe` Just (1, 5)
                    minMax [5, 4, 3, 2, 1] `shouldBe` Just (1, 5)
                    minMax ([] :: [Int]) `shouldBe` Nothing

            describe "minMaxBang" $ do
                it "finds the minimum and maximum of a list with strict evaluation" $ do
                    minMaxBang [1, 2, 3, 4, 5] `shouldBe` Just (1, 5)
                    minMaxBang [5, 4, 3, 2, 1] `shouldBe` Just (1, 5)
                    minMaxBang ([] :: [Int]) `shouldBe` Nothing
