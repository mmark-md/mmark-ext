{-# LANGUAGE OverloadedStrings #-}

module Text.MMark.Extension.FootnotesSpec (spec) where

import Data.Text (Text)
import Test.Hspec
import Text.MMark qualified as MMark
import Text.MMark.Extension.Footnotes
import Text.MMark.Extension.TestUtils

spec :: Spec
spec =
  describe "footnotes" $ do
    let to = withExt footnotes
    context "when link has no scheme"
      $ it "has no effect"
      $ "Link [link](1)."
        `to` "<p>Link <a href=\"1\">link</a>.</p>\n"
    context "when link has not \"footnote\" scheme"
      $ it "has no effect"
      $ "Link [link](https:1)"
        `to` "<p>Link <a href=\"https:1\">link</a></p>\n"
    context "when link has \"footnote\" scheme"
      $ it "transforms the link correctly"
      $ "Link [link](footnote:1)"
        `to` "<p>Link <a href=\"#fn1\" id=\"fnref1\"><sup>1</sup></a></p>\n"
    context "when block quotes are not formatted correctly"
      $ it "has no effect"
      $ "> blah"
        `to` "<blockquote>\n<p>blah</p>\n</blockquote>\n"
    context "when block quotes are formatted correctly"
      $ it "transforms them into footnotes"
      $ "> footnotes\n>\n> 1. Something.\n"
        `to` "<ol>\n<li id=\"fn1\">\nSomething.\n<a href=\"#fnref1\">↩</a></li>\n</ol>\n"
    context "validation" $ do
      it "accepts a document whose footnotes all line up" $
        check "Text [1](footnote:1).\n\n> footnotes\n>\n> 1. The note.\n"
          `shouldReturn` []
      it "reports a reference to a footnote that does not exist" $
        check "Text [2](footnote:2).\n\n> footnotes\n>\n> 1. The note.\n"
          `shouldReturn` ["1:6: there is no footnote 2", "5:6: nothing refers to footnote 1"]
      it "reports a footnote nothing refers to" $
        check "Text.\n\n> footnotes\n>\n> 1. Orphan.\n"
          `shouldReturn` ["5:6: nothing refers to footnote 1"]
      it "reports a footnote that is referred to more than once" $
        check "A [1](footnote:1) and B [1](footnote:1).\n\n> footnotes\n>\n> 1. N.\n"
          `shouldReturn` [ "1:25: footnote 1 is referred to more than once, which would give the references the same id"
                         ]
      it "reports a reference whose path is not a number" $
        check "Text [x](footnote:abc).\n\n> footnotes\n>\n> 1. N.\n"
          `shouldReturn` [ "1:6: a footnote reference must have a single number as its path",
                           "5:6: nothing refers to footnote 1"
                         ]
      it "reaches references nested inside other inlines" $
        check "T [1](footnote:1) *and [2](footnote:2)*.\n\n> footnotes\n>\n> 1. A.\n> 2. B.\n"
          `shouldReturn` []
      it "reports every problem exactly once" $
        check "[9](footnote:9)\n\nSome text.\n\nMore text.\n"
          `shouldReturn` ["1:1: there is no footnote 9"]
      -- The footnotes of the second section are the same numbers as those
      -- of the first, so they are not counted twice; the document is
      -- already reported as having more than one section.
      it "reports a second footnote section" $
        check "> footnotes\n>\n> 1. A.\n\n> footnotes\n>\n> 1. B.\n"
          `shouldReturn` [ "3:6: nothing refers to footnote 1",
                           "5:1: there is more than one footnote section"
                         ]

-- | Validate the footnotes of a document, returning one @line:col: message@
-- string per reported problem.
check :: Text -> IO [Text]
check input = do
  Right doc <- pure (MMark.parse "" input)
  checkErrors (validateFootnotes (MMark.runScanner footnoteScanner doc)) input
