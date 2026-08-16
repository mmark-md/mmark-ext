{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      :  Text.MMark.Extension.PunctuationPrettifier
-- Copyright   :  © 2018–present Mark Karpov
-- License     :  BSD 3 clause
--
-- Maintainer  :  Mark Karpov <markkarpov92@gmail.com>
-- Stability   :  experimental
-- Portability :  portable
--
-- Punctuation prettifier.
module Text.MMark.Extension.PunctuationPrettifier
  ( punctuationPrettifier,
  )
where

import Data.Char (isSpace)
import Data.Text (Text)
import Data.Text qualified as T
import Text.MMark.Trans (Bni, Inline (..), Trans)
import Text.MMark.Trans qualified as Trans

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
punctuationPrettifier :: Bni -> Trans Bni
punctuationPrettifier = Trans.bottomUpInlines $ \case
  Plain spn txt -> return (Plain spn (T.unfoldr gen (True, txt)))
  other -> return other

gen ::
  -- | Whether the previous character was a space and remaining input
  (Bool, Text) ->
  -- | Next generated char and the state
  Maybe (Char, (Bool, Text))
gen (s, i) =
  case T.uncons i of
    Nothing -> Nothing
    Just ('.', i') ->
      case T.splitAt 2 i' of
        ("..", i'') -> Just ('…', (False, i''))
        _ -> Just ('.', (False, i'))
    Just ('-', i') ->
      case T.splitAt 2 i' of
        ("--", i'') -> Just ('—', (False, i''))
        _ ->
          case T.splitAt 1 i' of
            ("-", i'') -> Just ('–', (False, i''))
            _ -> Just ('-', (False, i'))
    Just ('\"', i') ->
      if s -- whether previous character was a space character
        then Just ('“', (False, i'))
        else Just ('”', (False, i'))
    Just ('\'', i') ->
      if s
        then Just ('‘', (False, i'))
        else Just ('’', (False, i'))
    Just (ch, i') ->
      Just (ch, (isSpace ch, i'))
