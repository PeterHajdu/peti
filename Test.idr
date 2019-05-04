import Specdris.Spec

import Document
import Cursor

firstLine : String
firstLine = " line"

oneLiner : Document
oneLiner = MkDocument [firstLine]

secondLine : String
secondLine = " line2"

twoLiner : Document
twoLiner = MkDocument [firstLine, secondLine]

main : IO ()
main = spec $ do
  describe "document" $ do
    describe "insert" $ do
      describe "single line" $ do
        it "adds a single character to the beginning" $ do
          (insert oneLiner (MkCursor Z Z) 'a') `shouldBe` (MkDocument ["a line"])

        it "adds a single character to the middle" $ do
          (insert oneLiner (MkCursor (S Z) Z) 'a') `shouldBe` (MkDocument [" aline"])

        it "adds a single character to the end" $ do
          (insert oneLiner (MkCursor (length firstLine) Z) 'a') `shouldBe` (MkDocument [" linea"])

      describe "multiple lines" $ do
        it "adds a single character to the first line" $ do
          (insert twoLiner (MkCursor Z Z) 'a') `shouldBe` (MkDocument ["a line", secondLine])

        it "adds a single character to the second line" $ do
          (insert twoLiner (MkCursor Z (S Z)) 'a') `shouldBe` (MkDocument [firstLine, "a line2"])

      describe "cursor after bounds" $ do
        it "should not edit after the end of a line" $ do
          (insert oneLiner (MkCursor (1 + length firstLine) Z) 'a') `shouldBe` oneLiner

