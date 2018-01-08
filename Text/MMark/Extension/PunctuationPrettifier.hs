-- |
-- Module      :  Text.MMark.Extension.PunctuationPrettifier
-- Copyright   :  © 2018 Mark Karpov
-- License     :  BSD 3 clause
--
-- Maintainer  :  Mark Karpov <markkarpov92@gmail.com>
-- Stability   :  experimental
-- Portability :  portable
--
-- Punctuation prettifier.

{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Text.MMark.Extension.PunctuationPrettifier
  ( punctuationPrettifier )
where

import Text.MMark.Extension (Extension, Inline (..))
import qualified Data.Text as T
import qualified Text.MMark.Extension as Ext

-- | Prettify punctuation (only affects plain text in inlines):
--     * Replace @---@ with em-dash
--     * Replace @--@ with en-dash
--     * Replace @...@ with ellipsis
--     * Replace @'@ with apostrophe where makes sense

punctuationPrettifier :: Extension
punctuationPrettifier = Ext.inlineTrans $ \case
  Plain txt -> Plain
    . T.replace "--"  "–"
    . T.replace "---" "—"
    $ txt
  other -> other
