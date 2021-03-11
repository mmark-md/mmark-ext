{-# LANGUAGE OverloadedStrings #-}

module Text.MMark.Extension.FontAwesomeSpec (spec) where

import Test.Hspec
import Text.MMark.Extension.FontAwesome
import Text.MMark.Extension.TestUtils

spec :: Spec
spec =
  describe "fontAwesome" $ do
    let to = withExt fontAwesome
    context "when URI has the fa scheme" $
      it "produces the correct HTML" $ do
        "<fa:>" `to` "<p><a href=\"fa:\">fa:</a></p>\n"
        "<fa:user>" `to` "<p><span class=\"fa fa-user\"></span></p>\n"
        "<fa:user/lg>" `to` "<p><span class=\"fa fa-user fa-lg\"></span></p>\n"
        "<fa:quote-left/3x/pull-left/border>" `to` "<p><span class=\"fa fa-quote-left fa-3x fa-pull-left fa-border\"></span></p>\n"
    context "when URI has some other scheme" $
      it "produces the correct HTML" $
        "<https://example.org>" `to` "<p><a href=\"https://example.org\">https://example.org</a></p>\n"
    context "other elements" $
      it "not affected" $
        "Something." `to` "<p>Something.</p>\n"
