{-# LANGUAGE OverloadedStrings #-}

module Text.MMark.Extension.SkylightingSpec (spec) where

import Test.Hspec
import Text.MMark.Extension.Skylighting
import Text.MMark.Extension.TestUtils

spec :: Spec
spec =
  describe "skylighting" $ do
    let to = withExt skylighting
    context "when info string does not result in a successful lookup" $
      it "has no effect" $
        "```foo\nmain :: IO ()\nmain = return ()\n```\n" `to`
          "<pre><code class=\"language-foo\">main :: IO ()\nmain = return ()\n</code></pre>\n"
    context "with info string results in a successful lookup" $
      it "renders it correctly" $
        "```haskell\nmain :: IO ()\nmain = return ()\n```\n" `to`
          "<div class=\"source-code\"><pre><code class=\"language-haskell\"><span class=\"ot\">main ::</span><span> </span><span class=\"dt\">IO</span><span> ()</span>\n<span>main </span><span class=\"fu\">=</span><span> return ()</span>\n</code></pre></div>\n"
