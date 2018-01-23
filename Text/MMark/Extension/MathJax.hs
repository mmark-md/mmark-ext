-- |
-- Module      :  Text.MMark.Extension.MathJax
-- Copyright   :  © 2018 Mark Karpov
-- License     :  BSD 3 clause
--
-- Maintainer  :  Mark Karpov <markkarpov92@gmail.com>
-- Stability   :  experimental
-- Portability :  portable
--
-- Turn code spans and fenced code blocks into MathJax formulas.

{-# LANGUAGE OverloadedStrings #-}

module Text.MMark.Extension.MathJax
  ( mathJax )
where

import Control.Monad
import Data.Semigroup ((<>))
import Data.Text (Text)
import Lucid
import Text.MMark.Extension (Extension, Inline (..), Block (..))
import qualified Data.Text            as T
import qualified Text.MMark.Extension as Ext

-- | The extension allows to transform inline code spans into MathJax inline
-- spans and code blocks with the info string @\"mathjax\"@ (case-sensitive)
-- into MathJax display spans. Every line in such a code block will produce
-- a separate display span, i.e. a separate line with a formula (which is
-- probably what you want anyway).
--
-- The first argument is the character that must be the first and the last
-- character in code spans for them to be recognized as MathJax markup. If
-- 'Nothing' is passed instead of a char, we apply the transformation to all
-- code spans (useful for more academic articles that do not deal with
-- code).

mathJax
  :: Maybe Char -- ^ Starting\/ending character in MathJax inline spans
  -> Extension
mathJax mch = mathJaxSpan mch <> mathJaxBlock

-- | Turn code spans that start and end with a given character into MathJax
-- inline spans. If 'Nothing' is provided instead of a char, apply the
-- transformation to all code spans.

mathJaxSpan :: Maybe Char -> Extension
mathJaxSpan mch = Ext.inlineRender $ \old inline ->
  case inline of
    s@(CodeSpan txt) ->
      case mch of
        Nothing -> spn txt
        Just ch ->
          if T.length txt >= 2 && T.head txt == ch && T.last txt == ch
            then (spn . T.dropEnd 1 . T.drop 1) txt
            else old s
    other -> old other
  where
    spn :: Text -> Html ()
    spn x = span_ [class_ "math inline"] $
      "\\(" >> toHtml x >> "\\)"

-- | Turn code blocks with info string @\"mathjax\"@ into MathJax display
-- spans.

mathJaxBlock :: Extension
mathJaxBlock = Ext.blockRender $ \old block ->
  case block of
    b@(CodeBlock mlabel txt) ->
      if mlabel == Just "mathjax"
        then p_ . forM_ (T.lines txt) $ \x ->
               span_ [class_ "math display"] $
                 "\\[" >> toHtml x >> "\\]"
        else old b
    other -> old other
