-- |
-- Module      :  Text.MMark.Extension.Kbd
-- Copyright   :  © 2018–present Mark Karpov
-- License     :  BSD 3 clause
--
-- Maintainer  :  Mark Karpov <markkarpov92@gmail.com>
-- Stability   :  experimental
-- Portability :  portable
--
-- Introduce @kbd@ tags into resulting HTML document by wrapping content in
-- links with URL with @kbd@ scheme.

{-# LANGUAGE QuasiQuotes #-}

module Text.MMark.Extension.Kbd
  ( kbd )
where

import Lucid
import Text.MMark.Extension (Extension, Inline (..))
import Text.URI.QQ (scheme)
import qualified Text.MMark.Extension as Ext
import qualified Text.URI             as URI

-- | Introduce @kbd@ tags into resulting HTML document by wrapping content
-- in links with URL with @kbd@ scheme.
--
-- For example:
--
-- > To enable that mode press [Ctrl+A][kbd].
-- >
-- > [kbd]: kbd:
--
-- The use of reference-style links seems more aesthetically pleasant to the
-- author, but you can of course do somethnig like this instead:
--
-- > To enable that mode press [Ctrl+A](kbd:).

kbd :: Extension
kbd = Ext.inlineRender $ \old inline ->
  case inline of
    l@(Link inner uri _) ->
      if URI.uriScheme uri == Just [scheme|kbd|]
        then kbd_ (mapM_ old inner)
        else old l
    other -> old other
