{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

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
module Text.MMark.Extension.Footnotes
  ( -- * Rendering
    footnotes,

    -- * Validation
    Footnotes,
    footnoteScanner,
    validateFootnotes,
  )
where

import Control.Foldl qualified as L
import Control.Monad
import Data.Char (isDigit)
import Data.List (sort)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.Text (Text)
import Data.Text qualified as T
import Lens.Micro ((^.))
import Lucid
import Text.MMark qualified as MMark
import Text.MMark.Render (RenderExtension, getOis)
import Text.MMark.Render qualified as Render
import Text.MMark.Trans (Block (..), Bni, Inline (..), Span (..), Trans)
import Text.MMark.Trans qualified as Trans
import Text.URI qualified as URI
import Text.URI.Lens (uriPath)
import Text.URI.QQ (scheme)

----------------------------------------------------------------------------
-- Rendering

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
-- > >
-- > > 1. Here we have the footnote.
--
-- This extension only renders footnotes, it does not check that they make
-- sense. Pair it with 'validateFootnotes', which does.
footnotes :: RenderExtension
footnotes = footnoteRefs <> footnoteSection

-- | Create footnote references.
footnoteRefs :: RenderExtension
footnoteRefs = Render.inlineRender $ \old inline ->
  case inline of
    l@(Link _ _ uri _) ->
      case footnoteRef uri of
        Just n ->
          let x = renderIx n
           in a_ [fragmentHref (footnoteId x), id_ (referenceId x)] $
                sup_ (toHtml x)
        Nothing -> old l
    other -> old other

-- | Create a footnote section.
footnoteSection :: RenderExtension
footnoteSection = Render.blockRender $ \old block ->
  case block of
    b@(Blockquote _ [Paragraph _ (pOis, _), OrderedList _ i items]) ->
      if Render.asPlainText (getOis pOis) == footnoteLabel
        then do
          let startIndex = [start_ (renderIx i) | i /= 1]
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

----------------------------------------------------------------------------
-- Validation

-- | The footnotes of a document as collected by 'footnoteScanner'.
data Footnotes = Footnotes
  { -- | Span of every footnote section that was found, in order
    fnSections :: [Span],
    -- | Span of every footnote, by the number it is given
    fnDefined :: Map Word Span,
    -- | Span of every reference, by the number it refers to
    fnReferenced :: Map Word [Span],
    -- | Span of every reference we could not make sense of
    fnMalformed :: [Span]
  }

instance Semigroup Footnotes where
  x <> y =
    Footnotes
      { fnSections = fnSections x <> fnSections y,
        fnDefined = fnDefined x <> fnDefined y,
        fnReferenced = M.unionWith (<>) (fnReferenced x) (fnReferenced y),
        fnMalformed = fnMalformed x <> fnMalformed y
      }

instance Monoid Footnotes where
  mempty = Footnotes [] M.empty M.empty []

-- | Collect the footnotes of a document and the references to them, so that
-- 'validateFootnotes' can check that the two agree.
footnoteScanner :: L.Fold Bni Footnotes
footnoteScanner = MMark.scanner mempty $ \acc block ->
  acc <> scanSection block <> foldMap scanInlines block

-- | A check that reports every footnote that does not make sense. Every
-- problem is reported where it can be seen: a reference that leads nowhere
-- at the reference, a footnote that nothing refers to at the footnote.
--
-- > let fns = MMark.runScanner footnoteScanner doc
-- > case MMark.runCheck (validateFootnotes fns) doc of
-- >   Left errs -> putStrLn (errorBundlePretty errs)
-- >   Right () -> …
validateFootnotes :: Footnotes -> Trans ()
validateFootnotes Footnotes {..} = do
  forM_ (drop 1 fnSections) $ \spn ->
    Trans.report spn "there is more than one footnote section"
  forM_ fnMalformed $ \spn ->
    Trans.report
      spn
      "a footnote reference must have a single number as its path"
  forM_ (M.toAscList fnReferenced) $ \(n, spns) ->
    if M.member n fnDefined
      then forM_ (drop 1 (sort spns)) $ \spn ->
        Trans.report
          spn
          ( "footnote "
              <> renderIx n
              <> " is referred to more than once, which would give the"
              <> " references the same id"
          )
      else forM_ spns $ \spn ->
        Trans.report spn ("there is no footnote " <> renderIx n)
  forM_ (M.toAscList fnDefined) $ \(n, spn) ->
    unless (M.member n fnReferenced) $
      Trans.report spn ("nothing refers to footnote " <> renderIx n)

-- | Collect a footnote section, if this block is one.
scanSection :: Bni -> Footnotes
scanSection = \case
  Blockquote spn [Paragraph _ pInlines, OrderedList _ i items]
    | Trans.asPlainText pInlines == footnoteLabel ->
        mempty
          { fnSections = [spn],
            fnDefined = M.fromList (zip [i ..] (itemSpan <$> NE.toList items))
          }
  _ -> mempty
  where
    itemSpan = \case
      [] -> Span 0 0
      xs -> foldr1 Trans.spanUnion (Trans.blockSpan <$> xs)

-- | Collect the footnote references of a collection of inlines.
scanInlines :: NonEmpty Inline -> Footnotes
scanInlines = foldMap go
  where
    go = \case
      l@(Link spn inner uri _)
        | URI.uriScheme uri == Just [scheme|footnote|] ->
            case footnoteRef uri of
              Just n -> mempty {fnReferenced = M.singleton n [spn]}
              Nothing -> mempty {fnMalformed = [Trans.inlineSpan l]}
        | otherwise -> foldMap go inner
      Emphasis _ xs -> foldMap go xs
      Strong _ xs -> foldMap go xs
      Strikeout _ xs -> foldMap go xs
      Subscript _ xs -> foldMap go xs
      Superscript _ xs -> foldMap go xs
      Image _ xs _ _ -> foldMap go xs
      _ -> mempty

----------------------------------------------------------------------------
-- Helpers

-- | The number a footnote URI refers to, if it is a well-formed footnote
-- reference.
footnoteRef :: URI.URI -> Maybe Word
footnoteRef uri =
  if URI.uriScheme uri == Just [scheme|footnote|]
    then case uri ^. uriPath of
      [x'] ->
        let x = URI.unRText x'
         in if not (T.null x) && T.all isDigit x
              then Just (read (T.unpack x))
              else Nothing
      _ -> Nothing
    else Nothing

-- | The label that marks a block quote as the footnote section.
footnoteLabel :: Text
footnoteLabel = "footnotes"

renderIx :: Word -> Text
renderIx = T.pack . show

fragmentHref :: Text -> Attribute
fragmentHref = href_ . URI.render . Render.headerFragment

footnoteId :: Text -> Text
footnoteId x = "fn" <> x

referenceId :: Text -> Text
referenceId x = "fnref" <> x
