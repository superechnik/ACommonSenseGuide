import Test.Hspec ( hspec, describe, it, shouldBe )
import Chapter8 as C8 (intersect, findFirstDup)
import qualified Data.Map as Map

main :: IO ()
main = hspec $ do
    describe "Chapter8.intersect" $ do
        it "works with empty list first" $
            C8.intersect [1,2,3] [] `shouldBe` []
        it "works with empty list second" $            
            C8.intersect [] [1,2,3]  `shouldBe` []
        it "finds intersection true" $
            C8.intersect [1,2,3,4,5] [3,4,5,6,7] `shouldBe` [3,4,5]
        it "finds intersection false" $
            C8.intersect [1,2,3,4,5] [6,7,8,9,10] `shouldBe` []

    describe "Chapter8.findFirstDup" $ do 
        it "returns empty string for empty list" $
            C8.findFirstDup Map.empty [] `shouldBe` []
        it "finds the first duplicate best case" $ do 
            C8.findFirstDup Map.empty ["a","b","c","d", "c", "e","f"] `shouldBe` "c"
        it "finds the first duplicate worst case" $ do 
            C8.findFirstDup Map.empty ["a","b","c","d","e","f","g","g"] `shouldBe` "g"