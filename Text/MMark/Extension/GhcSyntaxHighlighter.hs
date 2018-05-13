-- |
-- Module      :  Text.MMark.Extension.GhcSyntaxHighlighter
-- Copyright   :  © 2018 Mark Karpov
-- License     :  BSD 3 clause
--
-- Maintainer  :  Mark Karpov <markkarpov92@gmail.com>
-- Stability   :  experimental
-- Portability :  portable
--
-- Use the @ghc-syntax-highlighter@ package to highlight Haskell code. This
-- module only works with GHC 8.4.1 and newer (with older versions the
-- extension just won't have any effect).
--
-- @since 0.2.1.0

{-# LANGUAGE CPP               #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Text.MMark.Extension.GhcSyntaxHighlighter
  ( ghcSyntaxHighlighter )
where

import Data.Text (Text)
import Lucid
import Text.MMark.Extension (Extension, Block (..))
import qualified Data.Text            as T
import qualified Text.MMark.Extension as Ext

#if __GLASGOW_HASKELL__ >= 804
import GHC.SyntaxHighlighter
#else
data Token
  = KeywordTok
  | PragmaTok
  | SymbolTok
  | VariableTok
  | ConstructorTok
  | OperatorTok
  | CharTok
  | StringTok
  | IntegerTok
  | RationalTok
  | CommentTok
  | SpaceTok
  | OtherTok
  deriving (Eq, Ord, Enum, Bounded, Show)
tokenizeHaskell :: Text -> Maybe [(Token, Text)]
tokenizeHaskell _ = Nothing
#endif

-- | Use the @ghc-syntax-highlighter@ package to highlight Haskell code. The
-- extension is applied only to code blocks with info string @\"haskell\"@.
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
-- overwrite its logic for code block with @\"haskell\"@ info string. So
-- place it on the left hand side of @('<>')@ or above
-- 'Text.MMark.Extension.Skylighting.skylighting' in the list passed to
-- 'Text.MMark.useExtensions'.

ghcSyntaxHighlighter :: Extension
ghcSyntaxHighlighter = Ext.blockRender $ \old block ->
  case block of
    cb@(CodeBlock (Just "haskell") txt) ->
      case tokenizeHaskell txt of
        Nothing -> old cb
        Just toks -> do
          div_ [class_ "source-code"]
            . pre_
            . code_ [class_ "language-haskell"]
            $ mapM_ tokenToHtml toks
          newline
    other -> old other
  where
    newline :: Html ()
    newline = "\n"

-- | Render a single 'Token'.

tokenToHtml :: (Token, Text) -> Html ()
tokenToHtml (tokenType, txt) =
  span_ [class_ rawClass | not (T.null rawClass)] (toHtml txt)
  where
    rawClass = tokenClass tokenType

-- | Return class corresponding to given 'TokenType'.

tokenClass :: Token -> Text
tokenClass = \case
  KeywordTok     -> "kw"
  PragmaTok      -> "pr"
  SymbolTok      -> "sy"
  VariableTok    -> "va"
  ConstructorTok -> "cr"
  OperatorTok    -> "op"
  CharTok        -> "ch"
  StringTok      -> "st"
  IntegerTok     -> "it"
  RationalTok    -> "ra"
  CommentTok     -> "co"
  SpaceTok       -> ""
  OtherTok       -> "ot"
