{-# LANGUAGE OverloadedStrings #-}

module Text.MMark.Extension.HeadingSpec (spec) where

import Data.Text (Text)
import Data.Text qualified as T
import Test.Hspec
import Text.MMark qualified as MMark
import Text.MMark.Extension.Heading
import Text.MMark.Extension.TestUtils

spec :: Spec
spec = do
  describe "checkHeadings" $ do
    it "reports a heading that skips a level" $
      headingErrors "# A\n\n### B"
        `shouldReturn` [ "3:1: this heading is of level 3, but the one before it is of level 1, so the outline of the document skips a level"
                       ]
    it "reports a second level 1 heading" $
      headingErrors "# A\n\n# B"
        `shouldReturn` ["3:1: there is more than one level 1 heading in this document"]
    it "reports two headings that get the same id" $
      headingErrors "# A\n\n## A"
        `shouldReturn` ["3:1: another heading is already given the id \"a\""]
    it "accepts a well formed outline" $
      headingErrors "# A\n\n## B\n\n### C\n\n## D" `shouldReturn` []
    it "reports each problem exactly once" $
      headingErrors "# A\n\nSome text.\n\nMore text.\n\n### B"
        `shouldReturn` [ "7:1: this heading is of level 3, but the one before it is of level 1, so the outline of the document skips a level"
                       ]

  describe "headingProblems" $ do
    it "finds nothing in a document with no headings" $
      problems "just some text" `shouldBe` []
    it "finds nothing in a well formed outline" $
      problems "# A\n\n## B\n\n### C\n\n## D" `shouldBe` []
    it "accepts an outline that comes back up several levels at once" $
      problems "# A\n\n## B\n\n### C\n\n## D\n\n# E" `shouldBe` ["title"]
    it "accepts a document that starts below level 1" $
      problems "## A\n\n### B" `shouldBe` []
    it "names the level a heading skips to and from" $
      problems "## A\n\n##### B" `shouldBe` ["skip"]
    it "finds a problem of each kind at once" $
      problems "# A\n\n### B\n\n# A" `shouldBe` ["skip", "title", "collision"]
    it "reports the problems in the order they appear" $
      problems "# A\n\n# B\n\n### C"
        `shouldBe` ["title", "skip"]
    it "finds a collision between headings of different levels" $
      problems "# Same\n\n## Same" `shouldBe` ["collision"]
    it "reports every heading after the first that shares an id" $
      problems "## A\n\n## A\n\n## A" `shouldBe` ["collision", "collision"]
    it "sees headings inside a block quote as part of the outline" $
      problems "# A\n\n> ### B" `shouldBe` []

-- | Scan a document for its headings and check them.
headingErrors :: Text -> IO [Text]
headingErrors input = do
  Right doc <- pure (MMark.parse "" input)
  checkErrors (checkHeadings (MMark.runScanner headingScanner doc)) input

-- | The problems of a document, each named by its kind so that a test does
-- not have to repeat the whole message.
problems :: Text -> [Text]
problems input = kindOf . snd <$> headingProblems (scan input)
  where
    scan t = case MMark.parse "" t of
      Left _ -> error "the test input does not parse"
      Right doc -> MMark.runScanner headingScanner doc
    kindOf msg
      | "skips a level" `T.isSuffixOf` msg = "skip"
      | "more than one level 1" `T.isInfixOf` msg = "title"
      | "already given the id" `T.isInfixOf` msg = "collision"
      | otherwise = msg
