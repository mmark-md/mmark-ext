{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Text.MMark.Extension.IconsSpec (spec) where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.Text (Text)
import Lucid
import Test.Hspec
import Text.MMark.Extension.Icons
import Text.MMark.Extension.TestUtils
import Text.URI.QQ (scheme)

spec :: Spec
spec = do
  describe "icons" $ do
    let to = withExt (icons table)
    it "puts the SVG of an autolink in place of it" $
      "<icon:github>"
        `to` "<p><span class=\"icon icon-github\" aria-hidden=\"true\"><svg id=\"gh\"></svg></span></p>\n"
    it "labels an icon that has link text" $
      "[GitHub](icon:github)"
        `to` "<p><span class=\"icon icon-github\" role=\"img\" aria-label=\"GitHub\"><svg id=\"gh\"></svg></span></p>\n"
    it "turns the rest of the path into classes" $
      "<icon:github/lg/pull-left>"
        `to` "<p><span class=\"icon icon-github icon-lg icon-pull-left\" aria-hidden=\"true\"><svg id=\"gh\"></svg></span></p>\n"
    it "leaves an icon it does not have as a link" $
      "<icon:nosuch>" `to` "<p><a href=\"icon:nosuch\">icon:nosuch</a></p>\n"
    it "leaves a link with no icon name alone" $
      "<icon:>" `to` "<p><a href=\"icon:\">icon:</a></p>\n"
    it "leaves a link of another scheme alone" $
      "<https://example.org>"
        `to` "<p><a href=\"https://example.org\">https://example.org</a></p>\n"
    it "leaves other inlines alone" $
      "Something." `to` "<p>Something.</p>\n"
  describe "iconsWith" $ do
    let to = withExt (iconsWith [scheme|fa|] "fa" table)
    it "uses the scheme and the prefix it is given" $
      "<fa:github>"
        `to` "<p><span class=\"fa fa-github\" aria-hidden=\"true\"><svg id=\"gh\"></svg></span></p>\n"
    it "leaves the scheme it replaces alone" $
      "<icon:github>" `to` "<p><a href=\"icon:github\">icon:github</a></p>\n"
  describe "checkIcons" $ do
    it "reports an icon it does not have" $
      transErrors (checkIcons table) "See <icon:nosuch> there"
        `shouldReturn` ["1:5: there is no icon called \"nosuch\""]
    it "reports a link that names no icon" $
      transErrors (checkIcons table) "See <icon:> there"
        `shouldReturn` ["1:5: this link names no icon"]
    it "says nothing about an icon it has" $
      transErrors (checkIcons table) "See <icon:github/lg> there"
        `shouldReturn` []
    it "says nothing about a link of another scheme" $
      transErrors (checkIcons table) "See <https://example.org> there"
        `shouldReturn` []
  describe "checkIconsWith"
    $ it "uses the scheme it is given"
    $ transErrors (checkIconsWith [scheme|fa|] table) "See <fa:nosuch> there"
      `shouldReturn` ["1:5: there is no icon called \"nosuch\""]

-- | An icon table with something recognizable in it. Raw SVG is how an icon
-- usually arrives, so that is what the table holds here.
table :: Map Text (Html ())
table = toHtmlRaw <$> M.fromList [("github", "<svg id=\"gh\"></svg>" :: Text)]
