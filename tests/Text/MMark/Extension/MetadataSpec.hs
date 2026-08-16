{-# LANGUAGE OverloadedStrings #-}

module Text.MMark.Extension.MetadataSpec (spec) where

import Test.Hspec
import Text.MMark qualified as MMark
import Text.MMark.Extension.Metadata
import Text.URI qualified as URI

spec :: Spec
spec = describe "metadataScanner" $ do
  it "finds the title, the lead, and the first image" $ do
    m <- scan "# Title\n\nThe lead here.\n\n![pic](/p.png)\n\nMore."
    metaTitle m `shouldBe` Just "Title"
    metaLead m `shouldBe` Just "The lead here."
    fmap URI.render (metaImage m) `shouldBe` Just "/p.png"
  it "counts words" $ do
    m <- scan "one two three four five"
    metaWords m `shouldBe` 5
  it "rounds the reading time up and never gives zero" $ do
    m <- scan "one two"
    readingTime 200 m `shouldBe` 1
  it "keeps the first of each thing" $ do
    m <- scan "# One\n\n# Two"
    metaTitle m `shouldBe` Just "One"
  where
    scan input = do
      Right doc <- pure (MMark.parse "" input)
      pure (MMark.runScanner metadataScanner doc)
