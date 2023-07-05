-- |
-- Module      :  Text.MMark.Extension.Comment
-- Copyright   :  © 2018–present Mark Karpov
-- License     :  BSD 3 clause
--
-- Maintainer  :  Mark Karpov <markkarpov92@gmail.com>
-- Stability   :  experimental
-- Portability :  portable
--
-- Turn paragraphs into comments by prefixing them with a certain sequence
-- of characters.
module Text.MMark.Extension.Comment
  ( commentParagraph,
  )
where

import Control.Monad
import Data.List.NonEmpty (NonEmpty (..))
import Data.Text (Text)
import Data.Text qualified as T
import Text.MMark.Extension (Block (..), Extension, Inline (..))
import Text.MMark.Extension qualified as Ext

-- | This extension removes top-level paragraphs starting with the given
-- sequence of non-markup characters.
commentParagraph ::
  -- | Sequence of characters that starts a comment
  Text ->
  Extension
commentParagraph commentPrefix = Ext.blockRender $ \old block ->
  case block of
    p@(Paragraph (ois, _)) ->
      case Ext.getOis ois of
        (Plain txt :| _) ->
          unless (commentPrefix `T.isPrefixOf` txt) $
            old p
        _ -> old p
    other -> old other
