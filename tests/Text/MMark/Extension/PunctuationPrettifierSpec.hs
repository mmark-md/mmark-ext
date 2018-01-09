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
        it "replaces ... with ellipsis" $
          "He forgot where he came from..." `to` "<p>He forgot where he came from…</p>\n"
        it "replaces --- with em dash" $
          "Here we go---at last." `to` "<p>Here we go—at last.</p>\n"
        it "replaces -- with en dash" $
          "Here we go -- at last." `to` "<p>Here we go – at last.</p>\n"
        it "replaces double quotes intelligently" $ do
          "\"Something\"" `to` "<p>“Something”</p>\n"
          "\"\"" `to` "<p>“”</p>\n"
        it "replaces single quotes intelligently" $ do
          "I'm doin' well, 'cause I care 'bout 'Big Jim'."
            `to` "<p>I’m doin’ well, ‘cause I care ‘bout ‘Big Jim’.</p>\n"
          "'Something'" `to` "<p>‘Something’</p>\n"
          "''" `to` "<p>‘’</p>\n"
    context "on other inlines" $
      it "has no effect" $
        "`code -- span`" `to` "<p><code>code -- span</code></p>\n"
