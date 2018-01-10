-- |
-- Module      :  Text.MMark.Extension.Common
-- Copyright   :  © 2017–2018 Mark Karpov
-- License     :  BSD 3 clause
--
-- Maintainer  :  Mark Karpov <markkarpov92@gmail.com>
-- Stability   :  experimental
-- Portability :  portable
--
-- Commonly useful extensions for MMark markdown processor.
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
-- > import Data.Default.Class
-- > import qualified Data.Text.IO                as T
-- > import qualified Data.Text.Lazy.IO           as TL
-- > import qualified Lucid                       as L
-- > import qualified Text.MMark                  as MMark
-- > import qualified Text.MMark.Extension.Common as Ext
-- >
-- > main :: IO ()
-- > main = do
-- >   let input = "input.md"
-- >   txt <- T.readFile input
-- >   case MMark.parse input txt of
-- >     Left errs -> putStrLn (MMark.parseErrorsPretty txt errs)
-- >     Right r ->
-- >       let toc = MMark.runScanner r (Ext.tocScanner 4)
-- >       in TL.writeFile "output.html"
-- >           . L.renderText
-- >           . MMark.render
-- >           . MMark.useExtensions
-- >               [ Ext.toc "toc" toc
-- >               , Ext.punctuationPrettifier
-- >               , Ext.obfuscateEmail "protected-email"
-- >               , Ext.fontAwesome ]
-- >           $ r

module Text.MMark.Extension.Common
  ( module Text.MMark.Extension.Comment
  , module Text.MMark.Extension.FontAwesome
  , module Text.MMark.Extension.Kbd
  , module Text.MMark.Extension.LinkTarget
  , module Text.MMark.Extension.ObfuscateEmail
  , module Text.MMark.Extension.PunctuationPrettifier
  , module Text.MMark.Extension.Skylighting
  , module Text.MMark.Extension.TableOfContents )
where

import Text.MMark.Extension.Comment
import Text.MMark.Extension.FontAwesome
import Text.MMark.Extension.Kbd
import Text.MMark.Extension.LinkTarget
import Text.MMark.Extension.ObfuscateEmail
import Text.MMark.Extension.PunctuationPrettifier
import Text.MMark.Extension.Skylighting
import Text.MMark.Extension.TableOfContents
