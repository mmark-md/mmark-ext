-- |
-- Module      :  Text.MMark.Extension.Comment
-- Copyright   :  © 2018 Mark Karpov
-- License     :  BSD 3 clause
--
-- Maintainer  :  Mark Karpov <markkarpov92@gmail.com>
-- Stability   :  experimental
-- Portability :  portable
--
-- Turn paragraphs into comments by prefixing them with a certain sequence
-- of characters.

module Text.MMark.Extension.Comment
  ( commentParagraph )
where

import Control.Monad
import Data.List.NonEmpty (NonEmpty (..))
import Data.Text (Text)
import Text.MMark.Extension (Extension, Block (..), Inline (..))
import qualified Data.Text            as T
import qualified Text.MMark.Extension as Ext

-- | This extension removes top-level paragraphs starting with the given
-- sequence of non-markup characters.

commentParagraph
  :: Text              -- ^ Sequence of characters that starts a comment
  -> Extension
commentParagraph commentPrefix = Ext.blockRender $ \old block ->
  case block of
    p@(Paragraph (ois, _)) ->
      case Ext.getOis ois of
        (Plain txt :| _) ->
          unless (commentPrefix `T.isPrefixOf` txt) $
            old p
        _ -> old p
    other -> old other
