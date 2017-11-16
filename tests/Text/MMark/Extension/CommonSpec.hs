{-# LANGUAGE OverloadedStrings #-}

module Text.MMark.Extension.CommonSpec (spec) where

import Data.Text (Text)
import Test.Hspec
import qualified Data.Text.Lazy              as TL
import qualified Lucid                       as L
import qualified Text.MMark                  as MMark
import qualified Text.MMark.Extension.Common as Ext

spec :: Spec
spec = parallel $ do
  describe "obfuscateEmail" $ do
    let to = withExt (Ext.obfuscateEmail "foo")
    context "when URI has the mailto scheme" $
      it "produces the correct HTML" $
        "<mailto:me@example.org>" `to` "<p><a href=\"javascript:void(0)\" class=\"foo\" data-email=\"me@example.org\">Enable JavaScript to see the email</a></p>\n"
    context "when URI has some other scheme" $
      it "produces the correct HTML" $
        "<https:example.org>" `to` "<p><a href=\"https:example.org\">https:example.org</a></p>\n"
    context "other elements" $
      it "not affected" $
        "Something." `to` "<p>Something.</p>\n"
  describe "fontAwesome" $ do
    let to = withExt Ext.fontAwesome
    context "when URI has the fa scheme" $
      it "produces the correct HTML" $ do
        "<fa:>" `to` "<p><a href=\"fa:\">fa:</a></p>\n"
        "<fa:user>" `to` "<p><span class=\"fa fa-user\"></span></p>\n"
        "<fa:user/lg>" `to` "<p><span class=\"fa fa-user fa-lg\"></span></p>\n"
        "<fa:quote-left/3x/pull-left/border>" `to` "<p><span class=\"fa fa-quote-left fa-3x fa-pull-left fa-border\"></span></p>\n"
    context "when URI has some other scheme" $
      it "produces the correct HTML" $
        "<https://example.org>" `to` "<p><a href=\"https://example.org/\">https://example.org/</a></p>\n"
    context "other elements" $
      it "not affected" $
        "Something." `to` "<p>Something.</p>\n"
----------------------------------------------------------------------------
-- Helpers

-- | Feed input into MMark parser, apply an extension, render the parsed
-- document and demand that it matches the given example.

withExt
  :: MMark.Extension   -- ^ MMark extension to use
  -> Text              -- ^ Input for the parser
  -> Text              -- ^ Expected output of the render
  -> Expectation
withExt ext input expected = do
  let Right doc = MMark.parse "" input
      actual = TL.toStrict
        . L.renderText
        . MMark.render
        . MMark.useExtension ext
        $ doc
  actual `shouldBe` expected
