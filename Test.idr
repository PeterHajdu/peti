import Specdris.Spec

main : IO ()
main = spec $ do
  describe "some test" $ do
    it "can fail" $ do
      1 `shouldBe` 2
    it "can succeed" $ do
      2 `shouldBe` 2
    it "can be pending" $ do
      pendingWith "a pending test"

