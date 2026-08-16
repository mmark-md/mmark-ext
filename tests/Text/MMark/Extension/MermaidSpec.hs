{-# LANGUAGE OverloadedStrings #-}

module Text.MMark.Extension.MermaidSpec (spec) where

import Data.Map.Strict qualified as M
import Data.Text.Lazy qualified as TL
import Lucid qualified as L
import Test.Hspec
import Text.MMark qualified as MMark
import Text.MMark.Extension.Mermaid
import Text.MMark.Extension.TestUtils

spec :: Spec
spec = do
  describe "mermaid" $ do
    it "renders a mermaid block for the browser" $
      withExt
        mermaid
        "```mermaid\ngraph TD;\n```"
        "<pre class=\"mermaid\">graph TD;\n</pre>\n"
    it "leaves another code block alone" $
      withExt
        mermaid
        "```haskell\nmain\n```"
        "<pre><code class=\"language-haskell\">main\n</code></pre>\n"
  describe "mermaidScanner and mermaidSvg" $ do
    it "puts the rendered diagram in place of the block" $ do
      Right doc <- pure (MMark.parse "" "```mermaid\ngraph TD;\n```")
      let svgs = M.map (const "<svg/>") (MMark.runScanner mermaidScanner doc)
      render (mermaidSvg svgs) doc
        `shouldBe` "<figure class=\"mermaid\"><svg/></figure>\n"
    it "leaves a block with no diagram as its source" $ do
      Right doc <- pure (MMark.parse "" "```mermaid\ngraph TD;\n```")
      render (mermaidSvg M.empty) doc
        `shouldBe` "<pre><code class=\"language-mermaid\">graph TD;\n</code></pre>\n"
  where
    render e = TL.toStrict . L.renderText . MMark.render e
