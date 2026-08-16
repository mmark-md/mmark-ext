{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      :  Text.MMark.Extension.LineHighlight
-- Copyright   :  © 2026–present Mark Karpov
-- License     :  BSD 3 clause
--
-- Maintainer  :  Mark Karpov <markkarpov92@gmail.com>
-- Stability   :  experimental
-- Portability :  portable
--
-- Point at the lines of a code block that the prose is about.
--
-- Write the lines to point at after the language in the info string:
--
-- > ```haskell {2,4-6}
-- > …
-- > ```
--
-- 'Text.MMark.Extension.Skylighting.skylighting' and
-- 'Text.MMark.Extension.GhcSyntaxHighlighter.ghcSyntaxHighlighter' read the
-- same specification and point at the lines themselves, around the tokens
-- they have coloured. Put either of them before this extension and it takes
-- the blocks whose language it knows; this one renders the rest, without
-- colouring but with the lines still pointed at.
--
-- @since 0.3.0.0
module Text.MMark.Extension.LineHighlight
  ( lineHighlight,
    parseLineSpec,
  )
where

import Data.Text (Text)
import Data.Text qualified as T
import Lucid
import Text.MMark.Extension.Internal (lineSpec, withLineHighlight)
import Text.MMark.Render (Block (..), RenderExtension)
import Text.MMark.Render qualified as Render

-- | Render a code block whose info string ends with a line specification,
-- giving the lines it names the class @\"highlighted-line\"@.
--
-- The language, if there is one, still becomes the @language-@ class of the
-- @\<code\>@ element, so this composes with a style sheet written for the
-- usual output.
lineHighlight :: RenderExtension
lineHighlight = Render.blockRender $ \old block ->
  case block of
    b@(CodeBlock _ (Just info) txt) ->
      case parseLineSpec info of
        Nothing -> old b
        Just (lang, ns) -> do
          pre_
            $ code_ (langAttr lang)
            $ mapM_ (line ns) (zip [1 :: Int ..] (T.lines txt))
          "\n"
    other -> old other
  where
    langAttr = \case
      Just l | not (T.null l) -> [class_ ("language-" <> l)]
      _ -> []
    line ns (n, t) = withLineHighlight ns n (toHtml (t <> "\n"))

-- | Split an info string into the language and the lines to point at.
-- Gives 'Nothing' when there is no line specification, so that an ordinary
-- code block is left to whatever renders it.
--
-- > parseLineSpec "haskell {2,4-6}" == Just (Just "haskell", [2,4,5,6])
parseLineSpec :: Text -> Maybe (Maybe Text, [Int])
parseLineSpec = lineSpec
