{-# LANGUAGE OverloadedStrings #-}

module Text.MMark.Extension.MathJaxSpec (spec) where

import Test.Hspec
import Text.MMark.Extension.MathJax
import Text.MMark.Extension.TestUtils

spec :: Spec
spec =
  describe "mathJax" $ do
    let to = withExt (mathJax Nothing)
        to' = withExt (mathJax (Just '$'))
    context "when span char is not specified" $
      it "transforms all code spans correctly" $
        "I've got `foo`."
          `to` "<p>I&#39;ve got <span class=\"math inline\">\\(foo\\)</span>.</p>\n"
    context "when span char is specified" $ do
      it "does not affect mismatching code spans" $
        "I've got `foo`."
          `to'` "<p>I&#39;ve got <code>foo</code>.</p>\n"
      it "transforms matching code spans correctly" $
        "I've got `$foo$`."
          `to'` "<p>I&#39;ve got <span class=\"math inline\">\\(foo\\)</span>.</p>\n"
    context "when code block is not labelled with \"mathjax\"" $
      it "does not affect it" $
        "```\nfoo\n```\n"
          `to` "<pre><code>foo\n</code></pre>\n"
    context "when code block is labelled with \"mathjax\"" $ do
      context "when code block contains a single line" $
        it "renders it correctly" $
          "```mathjax\nfoo\n```\n"
            `to` "<p><span class=\"math display\">\\[foo\\]</span></p>\n"
      context "when code block contains multiple lines" $
        it "renders it correctly" $
          "```mathjax\nfoo\nbar\n```\n"
            `to` "<p><span class=\"math display\">\\[foo\\]</span><span class=\"math display\">\\[bar\\]</span></p>\n"
