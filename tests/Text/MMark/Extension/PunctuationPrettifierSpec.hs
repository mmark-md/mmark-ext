{-# LANGUAGE OverloadedStrings #-}

module Text.MMark.Extension.PunctuationPrettifierSpec (spec) where

import Test.Hspec
import Text.MMark.Extension.PunctuationPrettifier
import Text.MMark.Extension.TestUtils

spec :: Spec
spec =
  describe "punctuationPrettifier" $ do
    let to = withExt punctuationPrettifier
    context "on plain inlines" $ do
        it "replaces -- with en dash" $
          "Here we go -- at last." `to` "<p>Here we go – at last.</p>\n"
        it "replaces --- with em dash" $
          "Here we go---at last." `to` "<p>Here we go—at last.</p>\n"
    context "on other inlines" $
      it "has no effect" $
        "`code -- span`" `to` "<p><code>code -- span</code></p>\n"
