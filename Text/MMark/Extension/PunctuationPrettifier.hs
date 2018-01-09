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

import Data.Char (isSpace)
import Data.Text (Text)
import Text.MMark.Extension (Extension, Inline (..))
import qualified Data.Text as T
import qualified Text.MMark.Extension as Ext

-- | Prettify punctuation (only affects plain text in inlines):
--
--     * Replace @...@ with ellipsis @…@
--     * Replace @---@ with em-dash @—@
--     * Replace @--@ with en-dash @–@
--     * Replace @\"@ with left double quote @“@ when previous character was
--       a space character, otherwise replace it with right double quote @”@
--     * Replace @'@ with left single quote @‘@ when previous character was
--       a space character, otherwise replace it with right single quote @’@
--       aka apostrophe

punctuationPrettifier :: Extension
punctuationPrettifier = Ext.inlineTrans $ \case
  Plain txt -> Plain (T.unfoldr gen (True, [], txt))
  other     -> other

gen
  :: (Bool, String, Text)
     -- ^ Whether the previous character was a space, characters pending
     -- insertion, and remaining input
  -> Maybe (Char, (Bool, String, Text))
     -- ^ Next generated char and the state
gen (s, x:xs, i) = Just (x, (s, xs, i))
gen (s, [],   i) =
  case T.uncons i of
    Nothing -> Nothing
    Just ('.', i') ->
      case T.splitAt 2 i' of
        ("..", i'') -> Just ('…', (False, [], i''))
        (xs,   i'') -> Just ('.', (False, T.unpack xs, i''))
    Just ('-', i') ->
      case T.splitAt 2 i' of
        ("--", i'') -> Just ('—', (False, [], i''))
        _ ->
          case T.splitAt 1 i' of
            ("-", i'') -> Just ('–', (False, [], i''))
            (xs,  i'') -> Just ('-', (False, T.unpack xs, i''))
    Just ('\"', i') ->
      if s -- whether previous character was a space character
        then Just ('“', (False, [], i'))
        else Just ('”', (False, [], i'))
    Just ('\'', i') ->
      if s
        then Just ('‘', (False, [], i'))
        else Just ('’', (False, [], i'))
    Just (ch, i') ->
      Just (ch, (isSpace ch, [], i'))
