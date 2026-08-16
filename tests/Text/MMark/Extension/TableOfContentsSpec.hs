{-# LANGUAGE OverloadedStrings #-}

module Text.MMark.Extension.TableOfContentsSpec (spec) where

import Data.Text (Text)
import Data.Text.IO qualified as TIO
import Data.Text.Lazy qualified as TL
import Lucid qualified as L
import Test.Hspec
import Text.MMark qualified as MMark
import Text.MMark.Extension.TableOfContents
import Text.MMark.Extension.TestUtils (summarize)
import Text.Megaparsec (errorBundlePretty)

spec :: Spec
spec =
  describe "toc" $ do
    it "works" $ do
      input <- TIO.readFile "data/toc.md"
      expected <- TIO.readFile "data/toc.html"
      Right doc <- pure (MMark.parse "" input)
      let headings = MMark.runScanner (tocScanner (> 1)) doc
      case MMark.runTrans (toc "toc" headings) doc of
        Left errs -> expectationFailure (errorBundlePretty errs)
        Right doc' ->
          (TL.toStrict . L.renderText . MMark.render mempty) doc'
            `shouldBe` expected
    it "leaves a code block with another label alone" $
      withToc (> 0) "toc" "# A\n\n```haskell\nx = 1\n```\n"
        `shouldBe` Right "<h1 id=\"a\">A</h1>\n<pre><code class=\"language-haskell\">x = 1\n</code></pre>\n"
    it "uses the label it is given" $
      withToc (> 0) "contents" "# A\n\n```contents\n```\n"
        `shouldBe` Right "<h1 id=\"a\">A</h1>\n<ul>\n<li>\n<a href=\"#a\">A</a>\n</li>\n</ul>\n"
    it "reports a table of contents with nothing to put in it" $
      withToc (> 1) "toc" "# A\n\n```toc\n```\n"
        `shouldBe` Left ["3:1: there are no headings to put in the table of contents"]
    it "reports a table of contents in a document with no headings at all" $
      withToc (> 0) "toc" "Some text.\n\n```toc\n```\n"
        `shouldBe` Left ["3:1: there are no headings to put in the table of contents"]
    it "says nothing about a document that asks for no table of contents" $
      withToc (> 1) "toc" "# A\n\nSome text.\n"
        `shouldBe` Right "<h1 id=\"a\">A</h1>\n<p>Some text.</p>\n"

-- | Build a table of contents out of the headings the predicate admits and
-- put it where the given label asks, giving either the problems reported or
-- the rendered document.
withToc :: (Int -> Bool) -> Text -> Text -> Either [Text] Text
withToc p label input =
  case MMark.parse "" input of
    Left _ -> error "the test input does not parse"
    Right doc ->
      case MMark.runTrans (toc label (MMark.runScanner (tocScanner p) doc)) doc of
        Left errs -> Left (summarize (errorBundlePretty errs))
        Right doc' ->
          Right (TL.toStrict (L.renderText (MMark.render mempty doc')))
