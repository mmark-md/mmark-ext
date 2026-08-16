{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      :  Text.MMark.Extension.Mermaid
-- Copyright   :  © 2026–present Mark Karpov
-- License     :  BSD 3 clause
--
-- Maintainer  :  Mark Karpov <markkarpov92@gmail.com>
-- Stability   :  experimental
-- Portability :  portable
--
-- Turn code blocks with the @mermaid@ info string into diagrams, either in
-- the browser or ahead of time.
--
-- @since 0.3.0.0
module Text.MMark.Extension.Mermaid
  ( -- * In the browser
    mermaid,

    -- * Ahead of time
    mermaidScanner,
    mermaidSvg,
  )
where

import Control.Foldl qualified as L
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.Text (Text)
import Lucid
import Text.MMark qualified as MMark
import Text.MMark.Render (Block (..), Bni, RenderExtension, Span)
import Text.MMark.Render qualified as Render

-- | Render a @mermaid@ code block as @\<pre class=\"mermaid\"\>@, which is
-- what the mermaid script in the page looks for.
mermaid :: RenderExtension
mermaid = Render.blockRender $ \old block ->
  case block of
    b@(CodeBlock _ mlabel txt) ->
      if mlabel == Just label
        then pre_ [class_ label] (toHtml txt) >> "\n"
        else old b
    other -> old other

-- | Collect the source of every @mermaid@ code block, by the span of the
-- block it came from.
--
-- Hand the result to whatever turns a diagram into an SVG, then give the
-- SVGs to 'mermaidSvg':
--
-- > srcs <- pure (MMark.runScanner mermaidScanner doc)
-- > svgs <- traverse mermaidCli srcs
-- > TL.putStr (renderText (MMark.render (mermaidSvg svgs) doc))
--
-- The span is the key because it is what tells two diagrams apart, even
-- two that contain exactly the same source.
mermaidScanner :: L.Fold Bni (Map Span Text)
mermaidScanner = MMark.scanner M.empty $ \acc block ->
  case block of
    CodeBlock spn (Just l) txt | l == label -> M.insert spn txt acc
    _ -> acc

-- | Put the given SVG in place of the @mermaid@ code block it was made
-- from. A block with no SVG is left as it is, so that a diagram that could
-- not be rendered is still visible as its source.
mermaidSvg :: Map Span Text -> RenderExtension
mermaidSvg svgs = Render.blockRender $ \old block ->
  case block of
    b@(CodeBlock spn (Just l) _)
      | l == label ->
          case M.lookup spn svgs of
            Just svg -> figure_ [class_ label] (toHtmlRaw svg) >> "\n"
            Nothing -> old b
    other -> old other

label :: Text
label = "mermaid"
