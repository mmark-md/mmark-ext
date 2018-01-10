-- |
-- Module      :  Text.MMark.Extension.Skylighting
-- Copyright   :  © 2018 Mark Karpov
-- License     :  BSD 3 clause
--
-- Maintainer  :  Mark Karpov <markkarpov92@gmail.com>
-- Stability   :  experimental
-- Portability :  portable
--
-- Use the Skylighting library to highlight code snippets.

{-# LANGUAGE OverloadedStrings #-}

module Text.MMark.Extension.Skylighting
  ( skylighting )
where

import Lucid (toHtmlRaw)
import Text.Blaze.Html.Renderer.Text
import Text.MMark.Extension (Extension, Block (..))
import qualified Data.Text            as T
import qualified Skylighting          as S
import qualified Text.MMark.Extension as Ext

-- | Use the @skylighting@ package to render code blocks with info strings
-- that result in a successful lookup from 'S.defaultSyntaxMap'.
--
-- The resulting markup is wrapped with spans as described in the docs for
-- 'S.formatHtmlInline'.

skylighting
  :: S.FormatOptions   -- ^ Skylighting formatting options
  -> Extension
skylighting fmtOpts = Ext.blockRender $ \old block ->
  case block of
    cb@(CodeBlock (Just infoString') txt) ->
      let tokenizerConfig = S.TokenizerConfig
            { S.syntaxMap   = S.defaultSyntaxMap
            , S.traceOutput = False }
          infoString = T.replace "-" " " infoString'
      in case S.lookupSyntax infoString S.defaultSyntaxMap of
           Nothing -> old cb
           Just syntax ->
             case S.tokenize tokenizerConfig syntax txt of
               Left _ -> old cb
               Right sourceLines -> toHtmlRaw . renderHtml $
                 S.formatHtmlBlock fmtOpts sourceLines
    other -> old other
