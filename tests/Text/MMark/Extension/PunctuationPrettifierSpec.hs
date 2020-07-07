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
      it "replaces double quotes intelligently" $
        "\"Something\"" `to` "<p>“Something”</p>\n"
      it "replaces double quotes intelligently (empty)" $
        "\"\"" `to` "<p>“”</p>\n"
      it "replaces single quotes intelligently" $ do
        "'Something'" `to` "<p>‘Something’</p>\n"
        "I'm doin' well, 'cause I care 'bout 'Big Jim'."
          `to` "<p>I’m doin’ well, ‘cause I care ‘bout ‘Big Jim’.</p>\n"
      it "replaces single quotes intelligently (empty)" $
        "''" `to` "<p>‘’</p>\n"
      it "a tricky test 1" $
        "Something-\"foo\"." `to` "<p>Something-”foo”.</p>\n"
      it "a tricky test 2" $
        "Something.--" `to` "<p>Something.–</p>\n"
    context "on other inlines" $
      it "has no effect" $
        "`code -- span`" `to` "<p><code>code -- span</code></p>\n"
