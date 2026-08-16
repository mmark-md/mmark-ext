{-# LANGUAGE OverloadedStrings #-}

module Text.MMark.Extension.GhcSyntaxHighlighterSpec (spec) where

import Data.Text qualified as T
import Test.Hspec
import Text.MMark.Extension.GhcSyntaxHighlighter
import Text.MMark.Extension.TestUtils

spec :: Spec
spec =
  describe "ghcSyntaxHighlighter" $ do
    let to = withExt ghcSyntaxHighlighter
    context "with info string is \"haskell\""
      $ it "renders it correctly"
      $ "```haskell\nmain :: IO ()\nmain = return ()\n```\n"
        `to` "<div class=\"source-code\"><pre><code class=\"language-haskell\"><span class=\"va\">main</span><span> </span><span class=\"sy\">::</span><span> </span><span class=\"cr\">IO</span><span> </span><span class=\"sy\">(</span><span class=\"sy\">)</span><span>\n</span><span class=\"va\">main</span><span> </span><span class=\"sy\">=</span><span> </span><span class=\"va\">return</span><span> </span><span class=\"sy\">(</span><span class=\"sy\">)</span><span>\n</span></code></pre></div>\n"
    context "when the info string ends with a line specification" $ do
      it "still recognizes the language, and points at the line" $
        "```haskell {2}\nmain :: IO ()\nmain = return ()\n```\n"
          `to` T.concat
            [ "<div class=\"source-code\"><pre><code class=\"language-haskell\">",
              "<span class=\"va\">main</span><span> </span><span class=\"sy\">::</span><span> </span><span class=\"cr\">IO</span><span> </span><span class=\"sy\">(</span><span class=\"sy\">)</span>\n",
              "<span class=\"highlighted-line\">",
              "<span class=\"va\">main</span><span> </span><span class=\"sy\">=</span><span> </span><span class=\"va\">return</span><span> </span><span class=\"sy\">(</span><span class=\"sy\">)</span>\n",
              "</span>",
              "</code></pre></div>\n"
            ]
      it "does not take a specification that names no line for one" $
        -- there is no such thing as pointing at nothing, so this is a
        -- malformed info string and the whole of it names the language
        "```haskell {}\nmain :: IO ()\nmain = return ()\n```\n"
          `to` "<pre><code class=\"language-haskell\">main :: IO ()\nmain = return ()\n</code></pre>\n"
      it "counts the lines the way Data.Text.lines does" $
        -- a trailing newline ends the last line, it does not start another,
        -- so there is no line 3 here to point at
        "```haskell {3}\nmain :: IO ()\nmain = return ()\n```\n"
          `to` "<div class=\"source-code\"><pre><code class=\"language-haskell\"><span class=\"va\">main</span><span> </span><span class=\"sy\">::</span><span> </span><span class=\"cr\">IO</span><span> </span><span class=\"sy\">(</span><span class=\"sy\">)</span>\n<span class=\"va\">main</span><span> </span><span class=\"sy\">=</span><span> </span><span class=\"va\">return</span><span> </span><span class=\"sy\">(</span><span class=\"sy\">)</span>\n</code></pre></div>\n"
      it "cuts a token that runs across lines at the newline" $
        -- the comment is one token spanning two lines; pointing at the
        -- second of them must not swallow the first
        "```haskell {2}\nx = 1\n{- a\nb -}\n```\n"
          `to` T.concat
            [ "<div class=\"source-code\"><pre><code class=\"language-haskell\">",
              "<span class=\"va\">x</span><span> </span><span class=\"sy\">=</span><span> </span><span class=\"it\">1</span>\n",
              "<span class=\"highlighted-line\"><span class=\"co\">{- a</span>\n</span>",
              "<span class=\"co\">b -}</span>\n",
              "</code></pre></div>\n"
            ]
      it "leaves a block of another language alone" $
        "```rust {1}\nfn main() {}\n```\n"
          `to` "<pre><code class=\"language-rust\">fn main() {}\n</code></pre>\n"
