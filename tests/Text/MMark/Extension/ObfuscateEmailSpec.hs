{-# LANGUAGE OverloadedStrings #-}

module Text.MMark.Extension.ObfuscateEmailSpec (spec) where

import Test.Hspec
import Text.MMark.Extension.ObfuscateEmail
import Text.MMark.Extension.TestUtils

spec :: Spec
spec =
  describe "obfuscateEmail" $ do
    let to = withExt (obfuscateEmail "foo")
    context "when URI has the mailto scheme" $
      it "produces the correct HTML" $
        "<mailto:me@example.org>" `to` "<p><a class=\"foo\" data-email=\"me@example.org\" href=\"javascript:void%280%29\">Enable JavaScript to see this email</a></p>\n"
    context "when URI has some other scheme" $
      it "produces the correct HTML" $
        "<https:example.org>" `to` "<p><a href=\"https:example.org\">https:example.org</a></p>\n"
    context "other elements" $
      it "not affected" $
        "Something." `to` "<p>Something.</p>\n"
