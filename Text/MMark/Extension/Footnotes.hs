-- |
-- Module      :  Text.MMark.Extension.Footnotes
-- Copyright   :  © 2018–present Mark Karpov
-- License     :  BSD 3 clause
--
-- Maintainer  :  Mark Karpov <markkarpov92@gmail.com>
-- Stability   :  experimental
-- Portability :  portable
--
-- An extension to add footnotes to your documents.
--
-- @since 0.1.1.0

{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Text.MMark.Extension.Footnotes
  ( footnotes )
where

import Control.Monad
import Data.Char (isDigit)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Text (Text)
import Lens.Micro ((^.))
import Lucid
import Text.MMark.Extension (Extension, Inline (..), Block (..), getOis)
import Text.URI.Lens (uriPath)
import Text.URI.QQ (scheme)
import qualified Data.List.NonEmpty   as NE
import qualified Data.Text            as T
import qualified Text.MMark.Extension as Ext
import qualified Text.URI             as URI

#if !MIN_VERSION_base(4,13,0)
import Data.Semigroup ((<>))
#endif

-- | The extension performs two transformations:
--
--     * It turns links with URIs with @footnote@ scheme and single path
--       piece consisting of a number into links to footnote references.
--     * It turns block quotes with the @\"footnotes\"@ label (see the
--       example below) into a footnote section.
--
-- > Here goes some text [1](footnote:1).
-- >
-- > > footnotes
-- >
-- >   1. Here we have the footnote.
--
-- The extension is not fully safe though in the sense that we can't check
-- that a footnote reference refers to an existing footnote and that
-- footnotes have corresponding references, or that they are present in the
-- document in the right order.

footnotes :: Extension
footnotes = footnoteRefs <> footnoteSection

-- | Create footnote references.

footnoteRefs :: Extension
footnoteRefs = Ext.inlineRender $ \old inline ->
  case inline of
    l@(Link _ uri _) ->
      if URI.uriScheme uri == Just [scheme|footnote|]
        then case uri ^. uriPath of
               [x'] ->
                 let x = URI.unRText x'
                 in if T.all isDigit x
                      then a_ [ fragmentHref (footnoteId x)
                              , id_ (referenceId x) ] $
                             sup_ (toHtml x)
                      else old l
               _ -> old l
        else old l
    other -> old other

-- | Create footnote section.

footnoteSection :: Extension
footnoteSection = Ext.blockRender $ \old block ->
  case block of
    b@(Blockquote [Paragraph (pOis, _), OrderedList i items]) ->
      if getOis pOis == Plain "footnotes" :| []
        then do let startIndex = [start_ (renderIx i) | i /= 1]
                    renderIx   = T.pack . show
                ol_ startIndex $ do
                  newline
                  forM_ (NE.zip (NE.iterate (+ 1) i) items) $ \(j, x) -> do
                    let j' = renderIx j
                    li_ [id_ (footnoteId j')] $ do
                      newline
                      mapM_ old x
                      a_ [fragmentHref (referenceId j')] "↩"
                    newline
                newline

        else old b
    other -> old other
    where
      newline = "\n"

fragmentHref :: Text -> Attribute
fragmentHref = href_ . URI.render . Ext.headerFragment

footnoteId :: Text -> Text
footnoteId x = "fn" <> x

referenceId :: Text -> Text
referenceId x = "fnref" <> x
