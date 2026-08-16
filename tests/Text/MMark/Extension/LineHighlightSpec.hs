{-# LANGUAGE OverloadedStrings #-}

module Text.MMark.Extension.LineHighlightSpec (spec) where

import Data.Text qualified as T
import Test.Hspec
import Text.MMark.Extension.LineHighlight
import Text.MMark.Extension.TestUtils

spec :: Spec
spec = do
  describe "parseLineSpec" $ do
    it "reads a single line" $
      parseLineSpec "haskell {2}" `shouldBe` Just (Just "haskell", [2])
    it "reads a range" $
      parseLineSpec "haskell {4-6}" `shouldBe` Just (Just "haskell", [4, 5, 6])
    it "reads a mixture" $
      parseLineSpec "haskell {2,4-6}" `shouldBe` Just (Just "haskell", [2, 4, 5, 6])
    it "works without a language" $
      parseLineSpec "{1}" `shouldBe` Just (Nothing, [1])
    it "gives nothing when there is no specification" $
      parseLineSpec "haskell" `shouldBe` Nothing
    it "gives nothing when the specification makes no sense" $ do
      parseLineSpec "haskell {x}" `shouldBe` Nothing
      parseLineSpec "haskell {6-4}" `shouldBe` Nothing
  describe "lineHighlight" $ do
    it "points at the line it is told to" $
      withExt
        lineHighlight
        "```haskell {2}\none\ntwo\n```"
        "<pre><code class=\"language-haskell\">one\n<span class=\"highlighted-line\">two\n</span></code></pre>\n"
    it "leaves a code block with no specification alone" $
      withExt
        lineHighlight
        "```haskell\none\n```"
        "<pre><code class=\"language-haskell\">one\n</code></pre>\n"
    it "renders a block of real code, pointing where it is told" $
      withExt
        lineHighlight
        ( T.unlines
            [ "```haskell {2,4-6}",
              "module Main (main) where",
              "",
              "import Data.List (sort & \"x\")",
              "main :: IO ()",
              "main = print (sort [3,1,2] <> [])",
              "-- done",
              "```"
            ]
        )
        ( T.concat
            [ "<pre><code class=\"language-haskell\">",
              "module Main (main) where\n",
              "<span class=\"highlighted-line\">\n</span>",
              "import Data.List (sort &amp; &quot;x&quot;)\n",
              "<span class=\"highlighted-line\">main :: IO ()\n</span>",
              "<span class=\"highlighted-line\">main = print (sort [3,1,2] &lt;&gt; [])\n</span>",
              "<span class=\"highlighted-line\">-- done\n</span>",
              "</code></pre>\n"
            ]
        )
    it "gives a block with no language no class to be styled by" $
      withExt
        lineHighlight
        "``` {1,3}\na\nb\nc\n```"
        ( T.concat
            [ "<pre><code>",
              "<span class=\"highlighted-line\">a\n</span>",
              "b\n",
              "<span class=\"highlighted-line\">c\n</span>",
              "</code></pre>\n"
            ]
        )
    it "ignores a line the block does not have" $
      withExt
        lineHighlight
        "```haskell {2,9}\na\nb\nc\n```"
        "<pre><code class=\"language-haskell\">a\n<span class=\"highlighted-line\">b\n</span>c\n</code></pre>\n"
