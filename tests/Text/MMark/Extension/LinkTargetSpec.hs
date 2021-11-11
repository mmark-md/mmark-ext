{-# LANGUAGE OverloadedStrings #-}

module Text.MMark.Extension.LinkTargetSpec (spec) where

import Test.Hspec
import Text.MMark.Extension.LinkTarget
import Text.MMark.Extension.TestUtils

spec :: Spec
spec =
  describe "linkTarget" $ do
    let to = withExt linkTarget
    context "when no link title provided" $
      it "has no effect" $
        "[link](/url)" `to` "<p><a href=\"/url\">link</a></p>\n"
    context "when link title does not start with a target" $
      it "has no effect" $
        "[link](/url 'something _blank')"
          `to` "<p><a href=\"/url\" title=\"something _blank\">link</a></p>\n"
    context "when link title starts with a target" $ do
      context "when there is nothing but the target in title" $
        it "works as intended, no title attribute produced" $
          "[link](/url '_blank')"
            `to` "<p><a target=\"_blank\" href=\"/url\">link</a></p>\n"
      context "when there is also a title" $
        it "works as intended, target is stripped from the title" $
          "[link](/url '_blank something')"
            `to` "<p><a target=\"_blank\" href=\"/url\" title=\"something\">link</a></p>\n"
