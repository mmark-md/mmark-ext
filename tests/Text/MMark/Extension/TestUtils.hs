{-# LANGUAGE OverloadedStrings #-}

module Text.MMark.Extension.TestUtils
  ( withExt,
    withTrans,
    transErrors,
    transErrorsM,
    checkErrors,
    summarize,
    withTempDir,
  )
where

import Control.Exception (bracket)
import Data.Char (isDigit)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Lucid qualified as L
import System.Directory
  ( createDirectory,
    getTemporaryDirectory,
    removeDirectoryRecursive,
    removeFile,
  )
import System.IO (hClose, openTempFile)
import Test.Hspec
import Text.MMark qualified as MMark
import Text.MMark.Trans (Bni, Trans, TransT)
import Text.Megaparsec (errorBundlePretty)

-- | Feed input into MMark parser, apply a render extension, render the
-- parsed document and demand that it matches the given example.
withExt ::
  -- | Render extension to use
  MMark.RenderExtension ->
  -- | Input for the parser
  Text ->
  -- | Expected output of the render
  Text ->
  Expectation
withExt ext input expected = do
  Right doc <- pure (MMark.parse "" input)
  render mempty doc `shouldBe` expected
  where
    render e = TL.toStrict . L.renderText . MMark.render (e <> ext)

-- | Like 'withExt', but applies a transformation instead.
withTrans ::
  -- | Transformation to apply
  (Bni -> Trans Bni) ->
  -- | Input for the parser
  Text ->
  -- | Expected output of the render
  Text ->
  Expectation
withTrans f input expected = do
  Right doc <- pure (MMark.parse "" input)
  case MMark.runTrans f doc of
    Left errs -> expectationFailure (errorBundlePretty errs)
    Right doc' ->
      (TL.toStrict . L.renderText . MMark.render mempty) doc'
        `shouldBe` expected

-- | Apply a transformation that is expected to report problems and return
-- one @line:col: message@ string per problem.
transErrors ::
  -- | Transformation to apply
  (Bni -> Trans Bni) ->
  -- | Input for the parser
  Text ->
  IO [Text]
transErrors f input = do
  Right doc <- pure (MMark.parse "" input)
  pure $ case MMark.runTrans f doc of
    Right _ -> []
    Left errs -> summarize (errorBundlePretty errs)

-- | Like 'transErrors', but for a transformation that needs 'IO'.
transErrorsM ::
  -- | Transformation to apply
  (Bni -> TransT IO Bni) ->
  -- | Input for the parser
  Text ->
  IO [Text]
transErrorsM f input = do
  Right doc <- pure (MMark.parse "" input)
  r <- MMark.runTransM f doc
  pure $ case r of
    Right _ -> []
    Left errs -> summarize (errorBundlePretty errs)

-- | Reduce a rendered error bundle to one @line:col: message@ string per
-- error, dropping the source excerpt megaparsec prints in between.
summarize :: String -> [Text]
summarize = go Nothing . fmap T.strip . T.lines . T.pack
  where
    go _ [] = []
    go cur (l : ls)
      | T.null l = go cur ls
      | isPos l = go (Just l) ls
      | "|" `T.isInfixOf` l = go cur ls
      | otherwise = case cur of
          Just p -> (p <> " " <> l) : go Nothing ls
          Nothing -> go Nothing ls
    isPos t = ":" `T.isSuffixOf` t && T.all (\c -> isDigit c || c == ':') t

-- | Run a check that is expected to report problems and return one
-- @line:col: message@ string per problem.
checkErrors ::
  -- | Check to run
  Trans a ->
  -- | Input for the parser
  Text ->
  IO [Text]
checkErrors c input = do
  Right doc <- pure (MMark.parse "" input)
  pure $ case MMark.runCheck c doc of
    Right _ -> []
    Left errs -> summarize (errorBundlePretty errs)

-- | Run an action in a fresh empty directory, which is removed afterwards.
withTempDir :: (FilePath -> IO a) -> IO a
withTempDir = bracket acquire removeDirectoryRecursive
  where
    -- 'openTempFile' is the only way base offers to get a name nothing else
    -- has taken, so take one and swap the file for a directory.
    acquire = do
      tmp <- getTemporaryDirectory
      (path, h) <- openTempFile tmp "mmark-ext-test"
      hClose h
      removeFile path
      createDirectory path
      return path
