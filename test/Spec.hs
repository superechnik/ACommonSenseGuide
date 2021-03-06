import Test.Hspec ( hspec, describe, it, shouldBe )
import Chapter8 as C8 (intersect, findFirstDup, findMissingLetter)
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

    describe "Chapter8.findMissingLetter" $ do 
        it "returns an empty string for an empty string" $
            C8.findMissingLetter "" `shouldBe` ""
        it "returns the missing letter or the first if multiple" $
            C8.findMissingLetter "the quick brown box jumps over a lazy dog" `shouldBe` "f" 
        it "still works with mixed case" $
            C8.findMissingLetter "tHe QUIck Brown Box Jumps over A lazy Dog" `shouldBe` "f"