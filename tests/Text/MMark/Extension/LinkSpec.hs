{-# LANGUAGE OverloadedStrings #-}

module Text.MMark.Extension.LinkSpec (spec) where

import Data.ByteString qualified as B
import Data.IORef
import Data.Text (Text)
import Data.Text qualified as T
import System.Directory (createDirectory)
import System.FilePath ((</>))
import Test.Hspec
import Text.MMark qualified as MMark
import Text.MMark.Extension.Link
import Text.MMark.Extension.TestUtils
import Text.URI (URI)
import Text.URI qualified as URI

spec :: Spec
spec = do
  describe "linkTarget" $ do
    let to = withExt linkTarget
    context "when no link title provided"
      $ it "has no effect"
      $ "[link](/url)" `to` "<p><a href=\"/url\">link</a></p>\n"
    context "when link title does not start with a target"
      $ it "has no effect"
      $ "[link](/url 'something _blank')"
        `to` "<p><a href=\"/url\" title=\"something _blank\">link</a></p>\n"
    context "when link title starts with a target" $ do
      context "when there is nothing but the target in title"
        $ it "works as intended, no title attribute produced"
        $ "[link](/url '_blank')"
          `to` "<p><a target=\"_blank\" rel=\"noopener noreferrer\" href=\"/url\">link</a></p>\n"
      context "when there is also a title"
        $ it "works as intended, target is stripped from the title"
        $ "[link](/url '_blank something')"
          `to` "<p><a target=\"_blank\" rel=\"noopener noreferrer\" href=\"/url\" title=\"something\">link</a></p>\n"
      context "when the target is not a new browsing context"
        $ it "does not add a rel attribute"
        $ "[link](/url '_self something')"
          `to` "<p><a target=\"_self\" href=\"/url\" title=\"something\">link</a></p>\n"
  describe "checkFragments" $ do
    it "accepts a link to a heading that exists" $
      fragmentErrors "# Real\n\n[go](#real)" `shouldReturn` []
    it "reports a link to a heading that does not" $
      fragmentErrors "# Real\n\n[go](#nope)"
        `shouldReturn` ["3:1: no heading of this document has the id \"nope\""]
    it "leaves a link with a scheme alone" $
      fragmentErrors "[go](https://example.org#nope)" `shouldReturn` []
    it "reports every bad fragment, once each" $
      fragmentErrors "# R\n\n[a](#x) and [b](#y)"
        `shouldReturn` [ "3:1: no heading of this document has the id \"x\"",
                         "3:13: no heading of this document has the id \"y\""
                       ]

  describe "checkLocalFiles" $ do
    it "accepts a link to a file that is there" $
      localErrors "[go](there.txt)" `shouldReturn` []
    it "reports a link to a file that is not" $
      localErrors "[go](nope.txt)"
        `shouldReturn` ["1:1: there is nothing at ./nope.txt"]
    it "accepts a link to a directory" $
      localErrors "[go](sub)" `shouldReturn` []
    it "accepts a link to a file in a subdirectory" $
      localErrors "[go](sub/deep.txt)" `shouldReturn` []
    it "ignores the fragment of a link to a file that is there" $
      localErrors "[go](there.txt#part)" `shouldReturn` []
    it "leaves a link with a scheme to checkExternal" $
      localErrors "[go](https://example.org/nope.txt)" `shouldReturn` []
    it "leaves a link that is only a fragment alone" $
      localErrors "[go](#part)" `shouldReturn` []
    it "checks images too" $
      localErrors "![x](nope.png)"
        `shouldReturn` ["1:1: there is nothing at ./nope.png"]
    it "reports every missing file, once each" $
      localErrors "[a](nope.txt) and [b](gone.txt)"
        `shouldReturn` [ "1:1: there is nothing at ./nope.txt",
                         "1:19: there is nothing at ./gone.txt"
                       ]

  describe "checkExternal" $ do
    it "accepts a link the action says is reachable" $
      externalErrors (const (pure True)) "[go](https://example.org)"
        `shouldReturn` []
    it "reports a link the action says is not" $
      externalErrors (const (pure False)) "[go](https://example.org)"
        `shouldReturn` ["1:1: cannot reach https://example.org"]
    it "hands the action the URI of the link" $ do
      seen <- asked (const True) "[go](https://example.org/a)"
      seen `shouldBe` ["https://example.org/a"]
    it "does not ask about a link with no scheme" $
      asked (const True) "[go](nope.txt)" `shouldReturn` []
    it "does not ask about a link that is only a fragment" $
      asked (const True) "[go](#part)" `shouldReturn` []
    it "asks about every external link, once each" $
      asked (const True) "[a](https://a.example) [b](https://b.example)"
        `shouldReturn` ["https://a.example", "https://b.example"]
    it "checks images too" $
      externalErrors (const (pure False)) "![x](https://example.org/a.png)"
        `shouldReturn` ["1:1: cannot reach https://example.org/a.png"]

-- | Scan a document for its header ids and check its fragments.
fragmentErrors :: Text -> IO [Text]
fragmentErrors input = do
  Right doc <- pure (MMark.parse "" input)
  transErrors (checkFragments (MMark.runScanner headerIdScanner doc)) input

-- | Check the local links of a document against a directory holding
-- @there.txt@ and @sub\/deep.txt@.
localErrors :: Text -> IO [Text]
localErrors input = withTempDir $ \dir -> do
  B.writeFile (dir </> "there.txt") ""
  createDirectory (dir </> "sub")
  B.writeFile (dir </> "sub" </> "deep.txt") ""
  errs <- transErrorsM (checkLocalFiles dir) input
  -- the messages name the base directory, which is a different one every
  -- run, so put something back that a test can be written against
  return (T.replace (T.pack dir) "." <$> errs)

-- | Check the external links of a document with the given action.
externalErrors :: (URI -> IO Bool) -> Text -> IO [Text]
externalErrors reachable = transErrorsM (checkExternal reachable)

-- | The URIs 'checkExternal' asked the action about, in order.
asked :: (URI -> Bool) -> Text -> IO [Text]
asked answer input = do
  ref <- newIORef []
  _ <-
    externalErrors
      (\uri -> modifyIORef' ref (URI.render uri :) >> pure (answer uri))
      input
  reverse <$> readIORef ref
