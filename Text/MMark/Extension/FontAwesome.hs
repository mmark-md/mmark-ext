{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

-- |
-- Module      :  Text.MMark.Extension.FontAwesome
-- Copyright   :  © 2017–present Mark Karpov
-- License     :  BSD 3 clause
--
-- Maintainer  :  Mark Karpov <markkarpov92@gmail.com>
-- Stability   :  experimental
-- Portability :  portable
--
-- Turn links into Font Awesome icons.
module Text.MMark.Extension.FontAwesome
  ( fontAwesome,
  )
where

import Data.Text qualified as T
import Lens.Micro ((^.))
import Lucid
import Text.MMark.Extension (Extension, Inline (..))
import Text.MMark.Extension qualified as Ext
import Text.URI qualified as URI
import Text.URI.Lens (uriPath)
import Text.URI.QQ (scheme)

-- | Insert @span@s with font awesome icons using autolinks like this:
--
-- > <fa:user>
--
-- This @user@ identifier is the name of the icon you want to insert. You
-- can also control the size of the icon like this:
--
-- > <fa:user/fw> -- fixed width
-- > <fa:user/lg> -- large
-- > <fa:user/2x>
-- > <fa:user/3x>
-- > <fa:user/4x>
-- > <fa:user/5x>
--
-- In general, all path components that go after the name of the icon will
-- be prefixed with @\"fa-\"@ and added as classes, so you can do a lot of
-- fancy stuff, see <http://fontawesome.io/examples/>:
--
-- > <fa:quote-left/3x/pull-left/border>
--
-- See also: <http://fontawesome.io>.
fontAwesome :: Extension
fontAwesome = Ext.inlineRender $ \old inline ->
  case inline of
    l@(Link _ uri _) ->
      if URI.uriScheme uri == Just [scheme|fa|]
        then case uri ^. uriPath of
          [] -> old l
          xs ->
            let g x = "fa-" <> URI.unRText x
             in span_
                  [(class_ . T.intercalate " ") ("fa" : fmap g xs)]
                  ""
        else old l
    other -> old other
