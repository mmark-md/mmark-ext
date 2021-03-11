{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      :  Text.MMark.Extension.Skylighting
-- Copyright   :  © 2018–present Mark Karpov
-- License     :  BSD 3 clause
--
-- Maintainer  :  Mark Karpov <markkarpov92@gmail.com>
-- Stability   :  experimental
-- Portability :  portable
--
-- Use the Skylighting library to highlight code snippets.
module Text.MMark.Extension.Skylighting
  ( skylighting,
  )
where

import Control.Monad
import Data.Text (Text)
import qualified Data.Text as T
import Lucid
import Skylighting (Token, TokenType (..))
import qualified Skylighting as S
import Text.MMark.Extension (Block (..), Extension)
import qualified Text.MMark.Extension as Ext

-- | Use the @skylighting@ package to render code blocks with info strings
-- that result in a successful lookup from 'S.defaultSyntaxMap'.
--
-- The resulting code block will be wrapped in a @div@ with class
-- @\"source-code\"@. The following @span@ classes can be used for styling:
--
--     * 'AlertTok'          = @\"al\"@
--     * 'AnnotationTok'     = @\"an\"@
--     * 'AttributeTok'      = @\"at\"@
--     * 'BaseNTok'          = @\"bn\"@
--     * 'BuiltInTok'        = @\"bu\"@
--     * 'CharTok'           = @\"ch\"@
--     * 'CommentTok'        = @\"co\"@
--     * 'CommentVarTok'     = @\"cv\"@
--     * 'ConstantTok'       = @\"cn\"@
--     * 'ControlFlowTok'    = @\"cf\"@
--     * 'DataTypeTok'       = @\"dt\"@
--     * 'DecValTok'         = @\"dv\"@
--     * 'DocumentationTok'  = @\"do\"@
--     * 'ErrorTok'          = @\"er\"@
--     * 'ExtensionTok'      = @\"ex\"@
--     * 'FloatTok'          = @\"fl\"@
--     * 'FunctionTok'       = @\"fu\"@
--     * 'ImportTok'         = @\"im\"@
--     * 'InformationTok'    = @\"in\"@
--     * 'KeywordTok'        = @\"kw\"@
--     * 'OperatorTok'       = @\"op\"@
--     * 'OtherTok'          = @\"ot\"@
--     * 'PreprocessorTok'   = @\"pp\"@
--     * 'RegionMarkerTok'   = @\"re\"@
--     * 'SpecialCharTok'    = @\"sc\"@
--     * 'SpecialStringTok'  = @\"ss\"@
--     * 'StringTok'         = @\"st\"@
--     * 'VariableTok'       = @\"va\"@
--     * 'VerbatimStringTok' = @\"vs\"@
--     * 'WarningTok'        = @\"wa\"@
skylighting :: Extension
skylighting = Ext.blockRender $ \old block ->
  case block of
    cb@(CodeBlock (Just infoString') txt) ->
      let tokenizerConfig =
            S.TokenizerConfig
              { S.syntaxMap = S.defaultSyntaxMap,
                S.traceOutput = False
              }
          infoString = T.replace "-" " " infoString'
       in case S.lookupSyntax infoString S.defaultSyntaxMap of
            Nothing -> old cb
            Just syntax ->
              case S.tokenize tokenizerConfig syntax txt of
                Left _ -> old cb
                Right ls -> do
                  div_ [class_ "source-code"]
                    . pre_
                    . code_ [class_ ("language-" <> infoString)]
                    . forM_ ls
                    $ \l -> do
                      mapM_ tokenToHtml l
                      newline
                  newline
    other -> old other
  where
    newline :: Html ()
    newline = "\n"

-- | Render a single 'Token'.
tokenToHtml :: Token -> Html ()
tokenToHtml (tokenType, txt) =
  span_ [class_ rawClass | not (T.null rawClass)] (toHtml txt)
  where
    rawClass = tokenClass tokenType

-- | Return class corresponding to given 'TokenType'.
tokenClass :: TokenType -> Text
tokenClass = \case
  KeywordTok -> "kw"
  DataTypeTok -> "dt"
  DecValTok -> "dv"
  BaseNTok -> "bn"
  FloatTok -> "fl"
  CharTok -> "ch"
  StringTok -> "st"
  CommentTok -> "co"
  OtherTok -> "ot"
  AlertTok -> "al"
  FunctionTok -> "fu"
  RegionMarkerTok -> "re"
  ErrorTok -> "er"
  ConstantTok -> "cn"
  SpecialCharTok -> "sc"
  VerbatimStringTok -> "vs"
  SpecialStringTok -> "ss"
  ImportTok -> "im"
  DocumentationTok -> "do"
  AnnotationTok -> "an"
  CommentVarTok -> "cv"
  VariableTok -> "va"
  ControlFlowTok -> "cf"
  OperatorTok -> "op"
  BuiltInTok -> "bu"
  ExtensionTok -> "ex"
  PreprocessorTok -> "pp"
  AttributeTok -> "at"
  InformationTok -> "in"
  WarningTok -> "wa"
  NormalTok -> ""
