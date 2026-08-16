{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      :  Text.MMark.Extension.Internal
-- Copyright   :  © 2026–present Mark Karpov
-- License     :  BSD 3 clause
--
-- Maintainer  :  Mark Karpov <markkarpov92@gmail.com>
-- Stability   :  experimental
-- Portability :  portable
--
-- Helpers shared by the extensions of this package.
module Text.MMark.Extension.Internal
  ( inlinesOf,
    lineSpec,
    infoStringParts,
    withLineHighlight,
  )
where

import Data.Char (isDigit)
import Data.List.NonEmpty qualified as NE
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Lucid
import Text.MMark.Trans (Bni, Inline (..))

-- | Every inline of a block, including the ones nested inside other
-- inlines and inside the blocks the block contains.
inlinesOf :: Bni -> [Inline]
inlinesOf = foldMap (concatMap go . NE.toList)
  where
    go i =
      i : case i of
        Emphasis _ xs -> nested xs
        Strong _ xs -> nested xs
        Strikeout _ xs -> nested xs
        Subscript _ xs -> nested xs
        Superscript _ xs -> nested xs
        Link _ xs _ _ -> nested xs
        Image _ xs _ _ -> nested xs
        _ -> []
    nested = concatMap go . NE.toList

-- | Split the info string of a code block into the language it names and
-- the lines it points at, as in @haskell {2,4-6}@.
--
-- Gives 'Nothing' when there is no line specification, so that a code block
-- written the usual way is left to whatever renders it.
lineSpec :: Text -> Maybe (Maybe Text, [Int])
lineSpec info = do
  let (before, rest) = T.breakOn "{" info
  spec <- T.stripSuffix "}" =<< T.stripPrefix "{" rest
  ns <- traverse range (T.splitOn "," (T.filter (/= ' ') spec))
  return (language before, concat ns)
  where
    range t = case T.splitOn "-" t of
      [a] -> (: []) <$> number a
      [a, b] -> do
        x <- number a
        y <- number b
        if x <= y then Just [x .. y] else Nothing
      _ -> Nothing
    number t =
      if not (T.null t) && T.all isDigit t
        then Just (read (T.unpack t))
        else Nothing

-- | Like 'lineSpec', but for an info string that need not carry a line
-- specification at all: one that does not simply points at no lines.
--
-- Every extension that renders a code block goes through this, so that a
-- language followed by a line specification is still recognized as that
-- language. Without it @haskell {2}@ looks like the name of a language
-- nobody has, and the block loses its syntax highlighting.
infoStringParts :: Text -> (Maybe Text, [Int])
infoStringParts info = fromMaybe (language info, []) (lineSpec info)

-- | Wrap the rendering of one line of a code block when the line is among
-- the ones pointed at.
withLineHighlight ::
  -- | The lines pointed at
  [Int] ->
  -- | The line being rendered, counting from one
  Int ->
  Html () ->
  Html ()
withLineHighlight ns n
  | n `elem` ns = span_ [class_ "highlighted-line"]
  | otherwise = id

-- | The language an info string names, if it names one.
language :: Text -> Maybe Text
language t =
  let l = T.strip t
   in if T.null l then Nothing else Just l
