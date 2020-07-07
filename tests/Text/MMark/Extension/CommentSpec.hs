{-# LANGUAGE OverloadedStrings #-}

module Text.MMark.Extension.CommentSpec (spec) where

import Test.Hspec
import Text.MMark.Extension.Comment
import Text.MMark.Extension.TestUtils

spec :: Spec
spec =
  describe "commentParagraph" $ do
    let to = withExt (commentParagraph "$$$")
    context "when it is the only content in document" $
      it "is removed" $
        "$$$ Here we go." `to` ""
    context "when it is intermixed with other paragraphs" $
      it "is removed" $
        "First.\n\n$$$Second.\n\nThird.\n" `to` "<p>First.</p>\n<p>Third.</p>\n"
    context "when it is not in plain text" $
      it "has no special effect" $
        "[$$$ link](/url) foo." `to` "<p><a href=\"/url\">$$$ link</a> foo.</p>\n"
