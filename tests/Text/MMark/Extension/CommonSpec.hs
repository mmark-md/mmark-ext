{-# LANGUAGE OverloadedStrings #-}

module Text.MMark.Extension.CommonSpec (spec) where

import Data.Default.Class
import Data.Text (Text)
import Test.Hspec
import qualified Data.Text.IO                as TIO
import qualified Data.Text.Lazy              as TL
import qualified Lucid                       as L
import qualified Text.MMark                  as MMark
import qualified Text.MMark.Extension.Common as Ext

spec :: Spec
spec = parallel $ do
  describe "toc" $
    it "works" $
      "data/toc.md" `withToc` "data/toc.html"
  describe "punctuationPrettifier" $ do
    let to = withExt (Ext.punctuationPrettifier def)
        ot = withExt $ Ext.punctuationPrettifier def
          { Ext.punctEnDash = False
          , Ext.punctEmDash = False }
    context "on plain inlines" $ do
      context "when enabeled" $ do
        it "replaces -- with en dash" $
          "Here we go -- at last." `to` "<p>Here we go – at last.</p>\n"
        it "replaces --- with em dash" $
          "Here we go---at last." `to` "<p>Here we go—at last.</p>\n"
      context "when disabled" $ do
        it "does not replace -- with en dash" $
          "Here we go -- at last." `ot` "<p>Here we go -- at last.</p>\n"
        it "does not replace --- with em dash" $
          "Here we go---at last." `ot` "<p>Here we go---at last.</p>\n"
    context "on other inlines" $
      it "has no effect" $
        "`code -- span`" `to` "<p><code>code -- span</code></p>\n"
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
-- | Similar to 'withExt' but specialized to test the 'Ext.toc' extension
-- and loads input and expected output from files.

withToc
  :: FilePath          -- ^ File containing input for the parser
  -> FilePath          -- ^ File containing expected output of the render
  -> Expectation
withToc ipath opath = do
  input    <- TIO.readFile ipath
  expected <- TIO.readFile opath
  let Right doc = MMark.parse "" input
      toc       = MMark.runScanner doc (Ext.tocScanner 6)
      actual = TL.toStrict
        . L.renderText
        . MMark.render
        . MMark.useExtension (Ext.toc "toc" toc)
        $ doc
  actual `shouldBe` expected
