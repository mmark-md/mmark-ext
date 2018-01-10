{-# LANGUAGE OverloadedStrings #-}

module Text.MMark.Extension.KbdSpec (spec) where

import Test.Hspec
import Text.MMark.Extension.Kbd
import Text.MMark.Extension.TestUtils

spec :: Spec
spec =
  describe "kbd" $ do
    let to = withExt kbd
    it "works" $
      "Press [Ctrl+A](kbd:)" `to` "<p>Press <kbd>Ctrl+A</kbd></p>\n"
