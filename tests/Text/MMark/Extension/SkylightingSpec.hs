{-# LANGUAGE OverloadedStrings #-}

module Text.MMark.Extension.SkylightingSpec (spec) where

import Data.Text qualified as T
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
        `to` "<div class=\"source-code\"><pre><code class=\"language-haskell\"><span class=\"ot\">main ::</span><span> </span><span class=\"dt\">IO</span><span> ()</span>\n<span>main </span><span class=\"ot\">=</span><span> </span><span class=\"fu\">return</span><span> ()</span>\n</code></pre></div>\n"
    context "when the info string ends with a line specification" $ do
      it "still recognizes the language, and points at the line" $
        "```haskell {2}\nmain :: IO ()\nmain = return ()\n```\n"
          `to` T.concat
            [ "<div class=\"source-code\"><pre><code class=\"language-haskell\">",
              "<span class=\"ot\">main ::</span><span> </span><span class=\"dt\">IO</span><span> ()</span>\n",
              "<span class=\"highlighted-line\">",
              "<span>main </span><span class=\"ot\">=</span><span> </span><span class=\"fu\">return</span><span> ()</span>\n",
              "</span>",
              "</code></pre></div>\n"
            ]
      it "points at every line a range names" $
        "```haskell {1-2}\nmain :: IO ()\nmain = return ()\n```\n"
          `to` T.concat
            [ "<div class=\"source-code\"><pre><code class=\"language-haskell\">",
              "<span class=\"highlighted-line\">",
              "<span class=\"ot\">main ::</span><span> </span><span class=\"dt\">IO</span><span> ()</span>\n",
              "</span>",
              "<span class=\"highlighted-line\">",
              "<span>main </span><span class=\"ot\">=</span><span> </span><span class=\"fu\">return</span><span> ()</span>\n",
              "</span>",
              "</code></pre></div>\n"
            ]
      it "does not take a specification that names no line for one" $
        -- there is no such thing as pointing at nothing, so this is a
        -- malformed info string and the whole of it names the language
        "```haskell {}\nmain :: IO ()\nmain = return ()\n```\n"
          `to` "<pre><code class=\"language-haskell\">main :: IO ()\nmain = return ()\n</code></pre>\n"
      it "leaves a block alone when the language is still not one it knows" $
        "```foo {1}\nmain :: IO ()\nmain = return ()\n```\n"
          `to` "<pre><code class=\"language-foo\">main :: IO ()\nmain = return ()\n</code></pre>\n"
      it "ignores a line the block does not have" $
        "```haskell {9}\nmain :: IO ()\nmain = return ()\n```\n"
          `to` "<div class=\"source-code\"><pre><code class=\"language-haskell\"><span class=\"ot\">main ::</span><span> </span><span class=\"dt\">IO</span><span> ()</span>\n<span>main </span><span class=\"ot\">=</span><span> </span><span class=\"fu\">return</span><span> ()</span>\n</code></pre></div>\n"
