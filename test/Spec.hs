import Test.Hspec
import Chapter8 as C8

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