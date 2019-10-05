import Test.Hspec
import Test.QuickCheck

import System.Random
import Lib

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "snake" $ do
    it "is inBox" $ do
        let c = (0, 0)
        inBox c `shouldBe` True
    it "is not inBox" $ do
        let c = (64, 0)
        inBox c `shouldBe` False
    it "extend" $ do
        let s = [(1,1)]
        let d = (0,-1)
        (extend s d) `shouldBe` [(1,0), (1,1)]
    it "move" $ do
        let s = Snake [(1,1), (1,0)] dDown
        (move s) `shouldBe` (Snake [(1,2), (1,1)] dDown)
    it "snake self collision" $ do
        let s = [(1,1), (1,0), (0,0)]
        let c = (1,0)
        (collides s c) `shouldBe` True 
    it "snake self non-collision" $ do
        let s = [(1,1), (1,0), (0,0)]
        let c = (5,5)
        (collides s c) `shouldBe` False
    it "apple collision" $ do
        let s = Snake [(1,1), (1,0), (0,0)] dDown
        let a = ((1,1), mkStdGen 12345)
        (fst $ collidesApple s a) `shouldNotBe` (fst a)
    it "apple non-collision" $ do
        let s = Snake [(1,1), (1,0), (0,0)] dDown
        let a = ((10,1), mkStdGen 12345)
        (fst $ collidesApple s a) `shouldBe` (fst a)
