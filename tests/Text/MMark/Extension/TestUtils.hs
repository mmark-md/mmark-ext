module Text.MMark.Extension.TestUtils
  ( withExt,
  )
where

import Data.Text (Text)
import Data.Text.Lazy qualified as TL
import Lucid qualified as L
import Test.Hspec
import Text.MMark qualified as MMark

-- | Feed input into MMark parser, apply an extension, render the parsed
-- document and demand that it matches the given example.
withExt ::
  -- | MMark extension to use
  MMark.Extension ->
  -- | Input for the parser
  Text ->
  -- | Expected output of the render
  Text ->
  Expectation
withExt ext input expected = do
  Right doc <- pure (MMark.parse "" input)
  let actual =
        TL.toStrict
          . L.renderText
          . MMark.render
          . MMark.useExtension ext
          $ doc
  actual `shouldBe` expected
