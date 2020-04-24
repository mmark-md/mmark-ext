{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

-- |
-- Module      :  Text.MMark.Extension.ObfuscateEmail
-- Copyright   :  © 2018–present Mark Karpov
-- License     :  BSD 3 clause
--
-- Maintainer  :  Mark Karpov <markkarpov92@gmail.com>
-- Stability   :  experimental
-- Portability :  portable
--
-- Obfuscate email addresses.
module Text.MMark.Extension.ObfuscateEmail
  ( obfuscateEmail,
  )
where

import Data.List.NonEmpty (NonEmpty (..))
import Data.Text (Text)
import Lucid
import Text.MMark.Extension (Extension, Inline (..))
import qualified Text.MMark.Extension as Ext
import qualified Text.URI as URI
import Text.URI.QQ (scheme, uri)

-- | This extension makes email addresses in autolinks be rendered as
-- something like this:
--
-- > <a class="protected-email"
-- >    data-email="something@example.org"
-- >    href="javascript:void(0)">Enable JavaScript to see this email</a>
--
-- You'll also need to include jQuery and this bit of JS code for the magic
-- to work:
--
-- > $(document).ready(function () {
-- >     $(".protected-email").each(function () {
-- >         var item = $(this);
-- >         var email = item.data('email');
-- >         item.attr('href', 'mailto:' + email);
-- >         item.html(email);
-- >     });
-- > });
obfuscateEmail ::
  -- | Name of class to assign to the links, e.g. @\"protected-email\"@
  Text ->
  Extension
obfuscateEmail class' = Ext.inlineRender $ \old inline ->
  case inline of
    l@(Link _ email mtitle) ->
      if URI.uriScheme email == Just [scheme|mailto|]
        then
          let txt = Plain "Enable JavaScript to see this email" :| []
              js = [uri|javascript:void(0)|]
           in with
                (old (Link txt js mtitle))
                [ class_ class',
                  data_
                    "email"
                    (URI.render email {URI.uriScheme = Nothing})
                ]
        else old l
    other -> old other
