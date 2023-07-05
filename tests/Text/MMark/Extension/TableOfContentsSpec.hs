{-# LANGUAGE OverloadedStrings #-}

module Text.MMark.Extension.TableOfContentsSpec (spec) where

import Data.Text.IO qualified as TIO
import Data.Text.Lazy qualified as TL
import Lucid qualified as L
import Test.Hspec
import Text.MMark qualified as MMark
import Text.MMark.Extension.TableOfContents

spec :: Spec
spec =
  describe "toc" $
    it "works" $ do
      input <- TIO.readFile "data/toc.md"
      expected <- TIO.readFile "data/toc.html"
      Right doc <- pure (MMark.parse "" input)
      let headings = MMark.runScanner doc (tocScanner (> 1))
          actual =
            TL.toStrict
              . L.renderText
              . MMark.render
              . MMark.useExtension (toc "toc" headings)
              $ doc
      actual `shouldBe` expected
