{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      :  Text.MMark.Extension.GhcSyntaxHighlighter
-- Copyright   :  © 2018–present Mark Karpov
-- License     :  BSD 3 clause
--
-- Maintainer  :  Mark Karpov <markkarpov92@gmail.com>
-- Stability   :  experimental
-- Portability :  portable
--
-- Use the @ghc-syntax-highlighter@ package to highlight Haskell code.
--
-- @since 0.2.1.0
module Text.MMark.Extension.GhcSyntaxHighlighter
  ( ghcSyntaxHighlighter,
  )
where

import Control.Monad (forM_)
import Data.Text (Text)
import Data.Text qualified as T
import GHC.SyntaxHighlighter
import Lucid
import Text.MMark.Extension.Internal (infoStringParts, withLineHighlight)
import Text.MMark.Render (Block (..), RenderExtension)
import Text.MMark.Render qualified as Ext

-- | Use the @ghc-syntax-highlighter@ package to highlight Haskell code. The
-- extension is applied only to code blocks with the info string
-- @\"haskell\"@.
--
-- The resulting code block will be wrapped in a @div@ with class
-- @\"source-code\"@. The following @span@ classes can be used for styling:
--
--     * 'KeywordTok'     = @\"kw\"@
--     * 'PragmaTok'      = @\"pr\"@
--     * 'SymbolTok'      = @\"sy\"@
--     * 'VariableTok'    = @\"va\"@
--     * 'ConstructorTok' = @\"cr\"@
--     * 'OperatorTok'    = @\"op\"@
--     * 'CharTok'        = @\"ch\"@
--     * 'StringTok'      = @\"st\"@
--     * 'IntegerTok'     = @\"it\"@
--     * 'RationalTok'    = @\"ra\"@
--     * 'CommentTok'     = @\"co\"@
--     * 'SpaceTok'       = no
--     * 'OtherTok'       = @\"ot\"@
--
-- To use with 'Text.MMark.Extension.Skylighting.skylighting' the extension
-- should be applied /after/ the
-- 'Text.MMark.Extension.Skylighting.skylighting' extension so it can
-- overwrite its logic for a code block with the @\"haskell\"@ info string.
-- So place it on the left hand side of @('<>')@ or above
-- 'Text.MMark.Extension.Skylighting.skylighting' in the list passed to
-- 'Text.MMark.useExtensions'.
--
-- The info string may end with a line specification, as in @haskell {2,4-6}@
-- (see 'Text.MMark.Extension.LineHighlight.lineHighlight'). It does not stop
-- the block from being recognized as Haskell, and the lines it names are
-- given the class @\"highlighted-line\"@ around the tokens of the line.
ghcSyntaxHighlighter :: RenderExtension
ghcSyntaxHighlighter = Ext.blockRender $ \old block ->
  case block of
    cb@(CodeBlock _ (Just infoString) txt)
      | (Just "haskell", highlighted) <- infoStringParts infoString ->
          case tokenizeHaskell txt of
            Nothing -> old cb
            Just toks -> do
              div_ [class_ "source-code"]
                . pre_
                . code_ [class_ "language-haskell"]
                $ if null highlighted
                  then mapM_ tokenToHtml toks
                  else forM_ (zip [1 ..] (tokenLines toks)) $ \(n, l) ->
                    withLineHighlight highlighted n $ do
                      mapM_ tokenToHtml l
                      newline
              newline
    other -> old other
  where
    newline :: Html ()
    newline = "\n"

-- | Split a token stream into the tokens of each line.
tokenLines :: [(Token, Text)] -> [[(Token, Text)]]
tokenLines = dropFinalEmpty . go []
  where
    dropFinalEmpty ls = case ls of
      (_ : _) | null (last ls) -> init ls
      _ -> ls
    go acc [] = [reverse acc]
    go acc ((tt, txt) : rest) =
      case T.splitOn "\n" txt of
        [] -> go acc rest
        [only] -> go (push tt only acc) rest
        (first : more) ->
          reverse (push tt first acc)
            : fmap (\m -> push tt m []) (init more)
              <> go (push tt (last more) []) rest
    -- an empty piece is not a token, it is where a newline was
    push tt t acc = if T.null t then acc else (tt, t) : acc

-- | Render a single 'Token'.
tokenToHtml :: (Token, Text) -> Html ()
tokenToHtml (tokenType, txt) =
  span_ [class_ rawClass | not (T.null rawClass)] (toHtml txt)
  where
    rawClass = tokenClass tokenType

-- | Return class corresponding to given 'TokenType'.
tokenClass :: Token -> Text
tokenClass = \case
  KeywordTok -> "kw"
  PragmaTok -> "pr"
  SymbolTok -> "sy"
  VariableTok -> "va"
  ConstructorTok -> "cr"
  OperatorTok -> "op"
  CharTok -> "ch"
  StringTok -> "st"
  IntegerTok -> "it"
  RationalTok -> "ra"
  CommentTok -> "co"
  SpaceTok -> ""
  OtherTok -> "ot"
