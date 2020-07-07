{-# LANGUAGE OverloadedStrings #-}

module Text.MMark.Extension.FootnotesSpec (spec) where

import Test.Hspec
import Text.MMark.Extension.Footnotes
import Text.MMark.Extension.TestUtils

spec :: Spec
spec =
  describe "footnotes" $ do
    let to = withExt footnotes
    context "when link has no scheme" $
      it "has no effect" $
        "Link [link](1)."
          `to` "<p>Link <a href=\"1\">link</a>.</p>\n"
    context "when link has not \"footnote\" scheme" $
      it "has no effect" $
        "Link [link](https:1)"
          `to` "<p>Link <a href=\"https:1\">link</a></p>\n"
    context "when link has \"footnote\" scheme" $
      it "transforms the link correctly" $
        "Link [link](footnote:1)"
          `to` "<p>Link <a href=\"#fn1\" id=\"fnref1\"><sup>1</sup></a></p>\n"
    context "when block quotes are not formatted correctly" $
      it "has no effect" $
        "> blah"
          `to` "<blockquote>\n<p>blah</p>\n</blockquote>\n"
    context "when block quotes are formatted correctly" $
      it "transforms them into footnotes" $
        "> footnotes\n\n  1. Something.\n"
          `to` "<ol>\n<li id=\"fn1\">\nSomething.\n<a href=\"#fnref1\">↩</a></li>\n</ol>\n"
