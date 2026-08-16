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
-- same time; it should give you an idea where to start:
--
-- > {-# LANGUAGE OverloadedStrings #-}
-- >
-- > module Main (main) where
-- >
-- > import           Control.Monad               ((>=>))
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
-- >     Right r -> do
-- >       let toc = MMark.runScanner (Ext.tocScanner (> 1)) r
-- >           fns = MMark.runScanner Ext.footnoteScanner r
-- >           trans = Ext.toc "toc" toc >=> Ext.punctuationPrettifier
-- >           renderExts = Ext.skylighting <> Ext.footnotes
-- >       case MMark.runCheck (Ext.validateFootnotes fns) r of
-- >         Left errs -> putStrLn (M.errorBundlePretty errs)
-- >         Right () -> return ()
-- >       case MMark.runTrans trans r of
-- >         Left errs -> putStrLn (M.errorBundlePretty errs)
-- >         Right r' ->
-- >           TL.writeFile "output.html"
-- >             . L.renderText
-- >             . MMark.render renderExts
-- >             $ r'
module Text.MMark.Extension.Common
  ( module Text.MMark.Extension.Comment,
    module Text.MMark.Extension.Emoji,
    module Text.MMark.Extension.Footnotes,
    module Text.MMark.Extension.GhcSyntaxHighlighter,
    module Text.MMark.Extension.Heading,
    module Text.MMark.Extension.Icons,
    module Text.MMark.Extension.Image,
    module Text.MMark.Extension.Kbd,
    module Text.MMark.Extension.LineHighlight,
    module Text.MMark.Extension.Link,
    module Text.MMark.Extension.MathJax,
    module Text.MMark.Extension.Mermaid,
    module Text.MMark.Extension.Metadata,
    module Text.MMark.Extension.Permalinks,
    module Text.MMark.Extension.PunctuationPrettifier,
    module Text.MMark.Extension.Skylighting,
    module Text.MMark.Extension.TableOfContents,
  )
where

import Text.MMark.Extension.Comment
import Text.MMark.Extension.Emoji
import Text.MMark.Extension.Footnotes
import Text.MMark.Extension.GhcSyntaxHighlighter
import Text.MMark.Extension.Heading
import Text.MMark.Extension.Icons
import Text.MMark.Extension.Image
import Text.MMark.Extension.Kbd
import Text.MMark.Extension.LineHighlight
import Text.MMark.Extension.Link
import Text.MMark.Extension.MathJax
import Text.MMark.Extension.Mermaid
import Text.MMark.Extension.Metadata
import Text.MMark.Extension.Permalinks
import Text.MMark.Extension.PunctuationPrettifier
import Text.MMark.Extension.Skylighting
import Text.MMark.Extension.TableOfContents
