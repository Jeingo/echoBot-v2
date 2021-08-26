import Test.Hspec

main :: IO ()
main = hspec test

test :: Spec
test = do
  describe "first test" $ do
    it "describe first test" $ 1 + 1 `shouldBe` 2
