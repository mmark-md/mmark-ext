-- |
-- Module      :  Text.MMark.Extension.Common
-- Copyright   :  © 2017–present Mark Karpov
-- License     :  BSD 3 clause
--
-- Maintainer  :  Mark Karpov <markkarpov92@gmail.com>
-- Stability   :  experimental
-- Portability :  portable
--
-- Commonly useful extensions for the MMark markdown processor.
--
-- We suggest using a qualified import, like this:
--
-- > import qualified Text.MMark.Extension.Common as Ext
--
-- Here is an example that uses several extensions from this module at the
-- same time, it should give you an idea where to start:
--
-- > {-# LANGUAGE OverloadedStrings #-}
-- >
-- > module Main (main) where
-- >
-- > import qualified Data.Text.IO                as T
-- > import qualified Data.Text.Lazy.IO           as TL
-- > import qualified Lucid                       as L
-- > import qualified Text.MMark                  as MMark
-- > import qualified Text.MMark.Extension.Common as Ext
-- > import qualified Text.Megaparsec             as M
-- >
-- > main :: IO ()
-- > main = do
-- >   let input = "input.md"
-- >   txt <- T.readFile input
-- >   case MMark.parse input txt of
-- >     Left bundle -> putStrLn (M.errorBundlePretty bundle)
-- >     Right r ->
-- >       let toc = MMark.runScanner r (Ext.tocScanner (> 1))
-- >       in TL.writeFile "output.html"
-- >           . L.renderText
-- >           . MMark.render
-- >           . MMark.useExtensions
-- >               [ Ext.toc "toc" toc
-- >               , Ext.punctuationPrettifier
-- >               , Ext.skylighting ]
-- >           $ r
module Text.MMark.Extension.Common
  ( module Text.MMark.Extension.Comment,
    module Text.MMark.Extension.FontAwesome,
    module Text.MMark.Extension.Footnotes,
    module Text.MMark.Extension.GhcSyntaxHighlighter,
    module Text.MMark.Extension.Kbd,
    module Text.MMark.Extension.LinkTarget,
    module Text.MMark.Extension.MathJax,
    module Text.MMark.Extension.ObfuscateEmail,
    module Text.MMark.Extension.PunctuationPrettifier,
    module Text.MMark.Extension.Skylighting,
    module Text.MMark.Extension.TableOfContents,
  )
where

import Text.MMark.Extension.Comment
import Text.MMark.Extension.FontAwesome
import Text.MMark.Extension.Footnotes
import Text.MMark.Extension.GhcSyntaxHighlighter
import Text.MMark.Extension.Kbd
import Text.MMark.Extension.LinkTarget
import Text.MMark.Extension.MathJax
import Text.MMark.Extension.ObfuscateEmail
import Text.MMark.Extension.PunctuationPrettifier
import Text.MMark.Extension.Skylighting
import Text.MMark.Extension.TableOfContents
