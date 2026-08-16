{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      :  Text.MMark.Extension.Permalinks
-- Copyright   :  © 2026–present Mark Karpov
-- License     :  BSD 3 clause
--
-- Maintainer  :  Mark Karpov <markkarpov92@gmail.com>
-- Stability   :  experimental
-- Portability :  portable
--
-- Give every heading a link to itself, so that a reader can get a URL that
-- points at the section they are looking at.
--
-- @since 0.3.0.0
module Text.MMark.Extension.Permalinks
  ( permalinks,
    permalinksWith,
  )
where

import Data.Text (Text)
import Lucid
import Lucid.Base (makeAttribute)
import Text.MMark.Render (Block (..), Ois, RenderExtension, getOis)
import Text.MMark.Render qualified as Render
import Text.URI qualified as URI

-- | Append to every heading a link to the id MMark gives that heading. The
-- link is labelled @\"#\"@ and given the class @\"permalink\"@, so that a
-- style sheet can show it only when the heading is hovered.
permalinks :: RenderExtension
permalinks = permalinksWith (const True) "permalink" Nothing "#"

-- | Like 'permalinks', but you choose which headings get a link, the class
-- it is given, what a screen reader makes of it, and what the reader sees.
--
-- The last of these is @'Html' ()@, so the link can be labelled with an
-- icon rather than a character:
--
-- > permalinksWith (\n -> n >= 2 && n <= 4) "anchor" Nothing linkIcon
--
-- A link nothing is to be said about is hidden from a screen reader, and
-- taken out of the order the keyboard walks: a link that is announced to
-- nobody is of no use to someone who has landed on it. Say what it is
-- instead to keep it in:
--
-- > permalinksWith (const True) "anchor" (Just "Link to this section") "#"
permalinksWith ::
  -- | Whether to give a heading of this level (1–6) a link
  (Int -> Bool) ->
  -- | Class to give the link
  Text ->
  -- | What a screen reader should say, if anything
  Maybe Text ->
  -- | What the reader sees
  Html () ->
  RenderExtension
permalinksWith p klass spoken shown = Render.blockRender $ \old block ->
  case block of
    Heading1 spn x | p 1 -> old (Heading1 spn (anchor x))
    Heading2 spn x | p 2 -> old (Heading2 spn (anchor x))
    Heading3 spn x | p 3 -> old (Heading3 spn (anchor x))
    Heading4 spn x | p 4 -> old (Heading4 spn (anchor x))
    Heading5 spn x | p 5 -> old (Heading5 spn (anchor x))
    Heading6 spn x | p 6 -> old (Heading6 spn (anchor x))
    other -> old other
  where
    anchor (ois, html) = (ois, html <> link ois)
    link :: Ois -> Html ()
    link ois =
      a_
        ( href_ (URI.render (Render.headerFragment (Render.headerId (getOis ois))))
            : class_ klass
            : how
        )
        shown
    how = case spoken of
      Just t -> [makeAttribute "aria-label" t]
      Nothing ->
        [ makeAttribute "aria-hidden" "true",
          makeAttribute "tabindex" "-1"
        ]
