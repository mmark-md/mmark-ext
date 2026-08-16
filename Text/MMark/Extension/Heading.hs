{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      :  Text.MMark.Extension.Heading
-- Copyright   :  © 2026–present Mark Karpov
-- License     :  BSD 3 clause
--
-- Maintainer  :  Mark Karpov <markkarpov92@gmail.com>
-- Stability   :  experimental
-- Portability :  portable
--
-- Checks on the headings of a document, which a parser cannot make on its
-- own because they concern the document as a whole: the outline the
-- headings form, and the ids they are given.
--
-- Scan the document first, then check what the scan collected:
--
-- > let hs = MMark.runScanner headingScanner doc
-- > MMark.runCheck (checkHeadings hs) doc
--
-- @since 0.3.0.0
module Text.MMark.Extension.Heading
  ( Headings,
    headingScanner,
    checkHeadings,
    headingProblems,
  )
where

import Control.Foldl qualified as L
import Data.List (sortOn)
import Data.Map.Strict qualified as M
import Data.Text (Text)
import Data.Text qualified as T
import Text.MMark qualified as MMark
import Text.MMark.Trans (Block (..), Bni, Span, Trans)
import Text.MMark.Trans qualified as Trans

-- | The headings of a document as collected by 'headingScanner'.
newtype Headings = Headings [(Span, Int, Text)]

instance Semigroup Headings where
  Headings x <> Headings y = Headings (x <> y)

instance Monoid Headings where
  mempty = Headings []

-- | Collect the headings of a document in the order they appear, with the
-- id each of them is given.
headingScanner :: L.Fold Bni Headings
headingScanner = MMark.scanner mempty $ \acc block ->
  acc <> heading block
  where
    heading b = case b of
      Heading1 spn x -> one spn 1 x
      Heading2 spn x -> one spn 2 x
      Heading3 spn x -> one spn 3 x
      Heading4 spn x -> one spn 4 x
      Heading5 spn x -> one spn 5 x
      Heading6 spn x -> one spn 6 x
      _ -> mempty
    one spn n x = Headings [(spn, n, Trans.headerId x)]

-- | A check that reports the problems 'headingProblems' finds.
checkHeadings :: Headings -> Trans ()
checkHeadings = mapM_ (uncurry Trans.report) . headingProblems

-- | The problems with the headings of a document:
--
--     * a heading that skips a level, such as a level 3 heading that
--       follows a level 1 one, which leaves a hole in the outline that
--       assistive technology relies on;
--     * a second level 1 heading, since a document has one title;
--     * two headings that MMark gives the same id, in which case every
--       link to one of them leads to the first.
headingProblems :: Headings -> [(Span, Text)]
headingProblems (Headings hs) =
  sortOn fst (skips <> extraTitles <> collisions)
  where
    skips =
      [ (spn, skipMessage prev n)
      | ((_, prev, _), (spn, n, _)) <- zip hs (drop 1 hs),
        n > prev + 1
      ]
    skipMessage prev n =
      "this heading is of level "
        <> tshow n
        <> ", but the one before it is of level "
        <> tshow prev
        <> ", so the outline of the document skips a level"
    extraTitles =
      [ (spn, "there is more than one level 1 heading in this document")
      | (spn, _, _) <- drop 1 [h | h@(_, 1, _) <- hs]
      ]
    collisions =
      [ (spn, "another heading is already given the id \"" <> i <> "\"")
      | (spn, _, i) <- hs,
        M.lookup i firstWithId /= Just spn
      ]
    firstWithId = M.fromListWith (\_ old -> old) [(i, spn) | (spn, _, i) <- hs]
    tshow :: Int -> Text
    tshow = T.pack . show
