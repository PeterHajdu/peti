import Specdris.Spec

import Document
import Cursor

firstLine : String
firstLine = " line"

filename : String
filename = "afile"

oneLiner : Document
oneLiner = MkDocument [firstLine] filename

secondLine : String
secondLine = " line2"

twoLiner : Document
twoLiner = MkDocument [firstLine, secondLine] filename

main : IO ()
main = spec $ do
  describe "document" $ do
    describe "insert" $ do
      describe "single line" $ do
        it "adds a single character to the beginning" $ do
          (insert oneLiner (MkCursor Z Z) 'a') `shouldBe` (MkDocument ["a line"] filename)

        it "adds a single character to the middle" $ do
          (insert oneLiner (MkCursor (S Z) Z) 'a') `shouldBe` (MkDocument [" aline"] filename)

        it "adds a single character to the end" $ do
          (insert oneLiner (MkCursor (length firstLine) Z) 'a') `shouldBe` (MkDocument [" linea"] filename)

      describe "multiple lines" $ do
        it "adds a single character to the first line" $ do
          (insert twoLiner (MkCursor Z Z) 'a') `shouldBe` (MkDocument ["a line", secondLine] filename)

        it "adds a single character to the second line" $ do
          (insert twoLiner (MkCursor Z (S Z)) 'a') `shouldBe` (MkDocument [firstLine, "a line2"] filename)

      describe "cursor after bounds" $ do
        it "should not edit after the end of a line" $ do
          (insert oneLiner (MkCursor (1 + length firstLine) Z) 'a') `shouldBe` oneLiner

