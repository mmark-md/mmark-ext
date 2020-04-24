{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

module Text.MMark.Extension.SkylightingSpec (spec) where

import Test.Hspec
import Text.MMark.Extension.Skylighting
import Text.MMark.Extension.TestUtils

spec :: Spec
spec =
  describe "skylighting" $ do
    let to = withExt skylighting
    context "when info string does not result in a successful lookup"
      $ it "has no effect"
      $ "```foo\nmain :: IO ()\nmain = return ()\n```\n"
        `to` "<pre><code class=\"language-foo\">main :: IO ()\nmain = return ()\n</code></pre>\n"
    context "with info string results in a successful lookup"
      $ it "renders it correctly"
      $ "```haskell\nmain :: IO ()\nmain = return ()\n```\n"
#if MIN_VERSION_skylighting(0,7,6)
          -- version 0.7.6 and later
           `to` "<div class=\"source-code\"><pre><code class=\"language-haskell\"><span class=\"ot\">main ::</span><span> </span><span class=\"dt\">IO</span><span> ()</span>\n<span>main </span><span class=\"ot\">=</span><span> </span><span class=\"fu\">return</span><span> ()</span>\n</code></pre></div>\n"
#elif MIN_VERSION_skylighting(0,7,4)
          -- versions 0.7.4 and 0.7.5
           `to` "<div class=\"source-code\"><pre><code class=\"language-haskell\"><span class=\"ot\">main ::</span><span> </span><span class=\"dt\">IO</span><span> ()</span>\n<span>main </span><span class=\"fu\">=</span><span> </span><span class=\"fu\">return</span><span> ()</span>\n</code></pre></div>\n"
#else
          -- versions older than 0.7.4
           `to` "<div class=\"source-code\"><pre><code class=\"language-haskell\"><span class=\"ot\">main ::</span><span> </span><span class=\"dt\">IO</span><span> ()</span>\n<span>main </span><span class=\"fu\">=</span><span> return ()</span>\n</code></pre></div>\n"
#endif
