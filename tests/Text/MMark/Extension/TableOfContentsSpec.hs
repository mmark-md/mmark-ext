{-# LANGUAGE OverloadedStrings #-}

module Text.MMark.Extension.TableOfContentsSpec (spec) where

import qualified Data.Text.IO as TIO
import qualified Data.Text.Lazy as TL
import qualified Lucid as L
import Test.Hspec
import qualified Text.MMark as MMark
import Text.MMark.Extension.TableOfContents

spec :: Spec
spec =
  describe "toc"
    $ it "works"
    $ do
      input <- TIO.readFile "data/toc.md"
      expected <- TIO.readFile "data/toc.html"
      let Right doc = MMark.parse "" input
          headings = MMark.runScanner doc (tocScanner (> 1))
          actual =
            TL.toStrict
              . L.renderText
              . MMark.render
              . MMark.useExtension (toc "toc" headings)
              $ doc
      actual `shouldBe` expected
