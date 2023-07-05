{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

-- |
-- Module      :  Text.MMark.Extension.LinkTarget
-- Copyright   :  © 2018–present Mark Karpov
-- License     :  BSD 3 clause
--
-- Maintainer  :  Mark Karpov <markkarpov92@gmail.com>
-- Stability   :  experimental
-- Portability :  portable
--
-- Specify the @target@ attribute of links in link titles. This allows us
-- to, e.g. make a link open in a new tab.
module Text.MMark.Extension.LinkTarget
  ( linkTarget,
  )
where

import Data.Foldable (asum)
import Data.Maybe (fromMaybe)
import Data.Text qualified as T
import Lucid
import Text.MMark.Extension (Extension, Inline (..))
import Text.MMark.Extension qualified as Ext

-- | When title of a link starts with the word @\"_blank\"@, @\"_self\"@,
-- @\"_parent\"@, or @\"_top\"@, it's stripped from title (as well as all
-- whitespace after it) and added as the value of @target@ attribute of the
-- resulting link.
--
-- For example:
--
-- > This [link](/url '_blank My title') opens in new tab.
linkTarget :: Extension
linkTarget = Ext.inlineRender $ \old inline ->
  case inline of
    l@(Link txt url (Just title)) -> fromMaybe (old l) $ do
      let f prefix =
            (prefix,) . T.stripStart
              <$> T.stripPrefix prefix title
      (prefix, title') <-
        asum $
          f <$> ["_blank", "_self", "_parent", "_top"]
      let mtitle = if T.null title' then Nothing else Just title'
      return $ with (old (Link txt url mtitle)) [target_ prefix]
    other -> old other
