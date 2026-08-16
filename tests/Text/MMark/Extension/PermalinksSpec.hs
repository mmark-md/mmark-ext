{-# LANGUAGE OverloadedStrings #-}

module Text.MMark.Extension.PermalinksSpec (spec) where

import Lucid
import Test.Hspec
import Text.MMark.Extension.Permalinks
import Text.MMark.Extension.TestUtils

spec :: Spec
spec = do
  describe "permalinks" $ do
    it "adds a link to the heading id" $
      withExt
        permalinks
        "# Title"
        "<h1 id=\"title\">Title<a href=\"#title\" class=\"permalink\" aria-hidden=\"true\" tabindex=\"-1\">#</a></h1>\n"
    it "works for every level" $
      withExt
        permalinks
        "###### Deep"
        "<h6 id=\"deep\">Deep<a href=\"#deep\" class=\"permalink\" aria-hidden=\"true\" tabindex=\"-1\">#</a></h6>\n"
    it "leaves other blocks alone" $
      withExt permalinks "Just text." "<p>Just text.</p>\n"
  describe "permalinksWith" $ do
    it "can be given another class and label" $
      withExt
        (permalinksWith (const True) "anchor" Nothing "\182")
        "# T"
        "<h1 id=\"t\">T<a href=\"#t\" class=\"anchor\" aria-hidden=\"true\" tabindex=\"-1\">\182</a></h1>\n"
    it "labels the link with the markup it is given" $
      withExt
        (permalinksWith (const True) "anchor" Nothing (toHtmlRaw ("<svg id=\"a\"></svg>" :: String)))
        "# T"
        "<h1 id=\"t\">T<a href=\"#t\" class=\"anchor\" aria-hidden=\"true\" tabindex=\"-1\"><svg id=\"a\"></svg></a></h1>\n"
    it "keeps a link a screen reader is told about" $
      withExt
        (permalinksWith (const True) "anchor" (Just "Link to this section") "#")
        "# T"
        "<h1 id=\"t\">T<a href=\"#t\" class=\"anchor\" aria-label=\"Link to this section\">#</a></h1>\n"
    it "gives a link only to the levels it is told to" $ do
      withExt (permalinksWith (\n -> n >= 2 && n <= 4) "anchor" Nothing "#") "# T" "<h1 id=\"t\">T</h1>\n"
      withExt
        (permalinksWith (\n -> n >= 2 && n <= 4) "anchor" Nothing "#")
        "## T"
        "<h2 id=\"t\">T<a href=\"#t\" class=\"anchor\" aria-hidden=\"true\" tabindex=\"-1\">#</a></h2>\n"
      withExt (permalinksWith (\n -> n >= 2 && n <= 4) "anchor" Nothing "#") "##### T" "<h5 id=\"t\">T</h5>\n"
