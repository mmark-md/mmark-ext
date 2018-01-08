module Text.MMark.Extension.TestUtils
  ( withExt )
where

import Data.Text (Text)
import Test.Hspec
import qualified Data.Text.Lazy as TL
import qualified Lucid          as L
import qualified Text.MMark     as MMark

-- | Feed input into MMark parser, apply an extension, render the parsed
-- document and demand that it matches the given example.

withExt
  :: MMark.Extension   -- ^ MMark extension to use
  -> Text              -- ^ Input for the parser
  -> Text              -- ^ Expected output of the render
  -> Expectation
withExt ext input expected = do
  let Right doc = MMark.parse "" input
      actual = TL.toStrict
        . L.renderText
        . MMark.render
        . MMark.useExtension ext
        $ doc
  actual `shouldBe` expected
