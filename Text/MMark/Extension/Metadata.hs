{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- |
-- Module      :  Text.MMark.Extension.Metadata
-- Copyright   :  © 2026–present Mark Karpov
-- License     :  BSD 3 clause
--
-- Maintainer  :  Mark Karpov <markkarpov92@gmail.com>
-- Stability   :  experimental
-- Portability :  portable
--
-- What a blog wants to know about a post: how long it is, how long it
-- takes to read, and what to put on the card that appears when it is
-- shared.
--
-- @since 0.3.0.0
module Text.MMark.Extension.Metadata
  ( Metadata (..),
    metadataScanner,
    readingTime,
  )
where

import Control.Foldl qualified as L
import Data.Text (Text)
import Data.Text qualified as T
import Text.MMark qualified as MMark
import Text.MMark.Extension.Internal (inlinesOf)
import Text.MMark.Trans (Block (..), Bni, Inline (..))
import Text.MMark.Trans qualified as Trans
import Text.URI (URI)

-- | What 'metadataScanner' finds out about a document.
data Metadata = Metadata
  { -- | Number of words in the prose of the document
    metaWords :: !Int,
    -- | Text of the first paragraph, for the description of a card
    metaLead :: Maybe Text,
    -- | URI of the first image, for the picture on a card
    metaImage :: Maybe URI,
    -- | Text of the first level 1 heading, for the title
    metaTitle :: Maybe Text
  }
  deriving (Eq, Show)

instance Semigroup Metadata where
  x <> y =
    Metadata
      { metaWords = metaWords x + metaWords y,
        metaLead = firstOf metaLead,
        metaImage = firstOf metaImage,
        metaTitle = firstOf metaTitle
      }
    where
      firstOf f = maybe (f y) Just (f x)

instance Monoid Metadata where
  mempty = Metadata 0 Nothing Nothing Nothing

-- | Scan a document for its 'Metadata'.
--
-- > let meta = MMark.runScanner metadataScanner doc
-- > putStrLn (show (readingTime 200 meta) <> " minute read")
metadataScanner :: L.Fold Bni Metadata
metadataScanner = MMark.scanner mempty $ \acc block ->
  acc <> ofBlock block
  where
    ofBlock block =
      mempty
        { metaWords = wordsIn block,
          metaLead = leadOf block,
          metaImage = imageOf block,
          metaTitle = titleOf block
        }
    wordsIn = length . T.words . T.unwords . fmap plainOf . inlinesOf
    plainOf = \case
      Plain _ t -> t
      CodeSpan _ t -> t
      _ -> ""
    leadOf = \case
      Paragraph _ xs -> Just (Trans.asPlainText xs)
      _ -> Nothing
    titleOf = \case
      Heading1 _ xs -> Just (Trans.asPlainText xs)
      _ -> Nothing
    imageOf block = case [uri | Image _ _ uri _ <- inlinesOf block] of
      (uri : _) -> Just uri
      [] -> Nothing

-- | How many minutes the document takes to read at the given number of
-- words per minute, rounded up, and never less than one.
readingTime ::
  -- | Words per minute, 200 to 250 for most readers
  Int ->
  -- | Collected metadata
  Metadata ->
  Int
readingTime wpm Metadata {..} =
  max 1 ((metaWords + wpm - 1) `div` wpm)
