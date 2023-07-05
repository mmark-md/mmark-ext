{-# LANGUAGE QuasiQuotes #-}

-- |
-- Module      :  Text.MMark.Extension.Kbd
-- Copyright   :  © 2018–present Mark Karpov
-- License     :  BSD 3 clause
--
-- Maintainer  :  Mark Karpov <markkarpov92@gmail.com>
-- Stability   :  experimental
-- Portability :  portable
--
-- Introduce @kbd@ tags by wrapping content in links with @kbd@ scheme.
module Text.MMark.Extension.Kbd
  ( kbd,
  )
where

import Lucid
import Text.MMark.Extension (Extension, Inline (..))
import Text.MMark.Extension qualified as Ext
import Text.URI qualified as URI
import Text.URI.QQ (scheme)

-- | Introduce @kbd@ tags by wrapping content in links with @kbd@ scheme.
--
-- For example:
--
-- > To enable that mode press [Ctrl+A][kbd].
-- >
-- > [kbd]: kbd:
--
-- The use of reference-style links seems more aesthetically pleasant to me,
-- but you can of course do somethnig like this instead:
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
