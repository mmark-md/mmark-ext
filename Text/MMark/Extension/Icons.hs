{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

-- |
-- Module      :  Text.MMark.Extension.Icons
-- Copyright   :  © 2026–present Mark Karpov
-- License     :  BSD 3 clause
--
-- Maintainer  :  Mark Karpov <markkarpov92@gmail.com>
-- Stability   :  experimental
-- Portability :  portable
--
-- Put an icon in a document by naming it: @\<icon:github\>@.
--
-- The icons are yours. You give 'icons' a table that says what each name
-- draws, and the SVG it finds there goes into the page:
--
-- > myIcons :: Map Text (Html ())
-- > myIcons = toHtmlRaw <$> M.fromList
-- >   [ ("github", "<svg viewBox=\"0 0 24 24\">…</svg>")
-- >   , ("envelope", "<svg viewBox=\"0 0 24 24\">…</svg>")
-- >   ]
--
-- Nothing else about the icons is this extension's business, so any SVG
-- will do, whoever drew it. The sets people usually take them from, with
-- the licence each one puts on its artwork:
--
--     * Font Awesome Free (CC BY 4.0), the largest of them
--     * Lucide (ISC) and Feather (MIT), which it forked from, both drawn as
--       strokes on a 24×24 grid
--     * Bootstrap Icons (MIT), Heroicons (MIT), Tabler Icons (MIT), and
--       Phosphor (MIT)
--     * Octicons (MIT), the ones GitHub uses
--     * Material Symbols (Apache 2.0)
--     * Simple Icons (CC0), for the logos of companies and projects, which
--       the general-purpose sets mostly do not carry
--
-- Or draw your own, export one from a design tool, or build it with the
-- Lucid combinators instead of pasting the markup: the table holds
-- @'Html' ()@, so it does not care where the SVG came from.
--
-- This package ships no icons of its own, because bundling artwork would
-- put someone else's licence and attribution on top of its own. Whichever
-- set you take from, check what its licence asks of you; the CC BY ones
-- want to be credited somewhere in your page.
--
-- @since 0.3.0.0
module Text.MMark.Extension.Icons
  ( -- * Rendering
    icons,
    iconsWith,

    -- * Checking
    checkIcons,
    checkIconsWith,
  )
where

import Data.List.NonEmpty (NonEmpty (..))
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.Text (Text)
import Data.Text qualified as T
import Lucid
import Lucid.Base (makeAttribute)
import Text.MMark.Extension.Internal (inlinesOf)
import Text.MMark.Render (Inline (..), RenderExtension)
import Text.MMark.Render qualified as Render
import Text.MMark.Trans (Bni, Trans)
import Text.MMark.Trans qualified as Trans
import Text.URI (RText, RTextLabel (..), URI)
import Text.URI qualified as URI
import Text.URI.QQ (scheme)

-- | Put the SVG of an icon in place of every link with the @icon@ scheme
-- that names one:
--
-- > <icon:github>
--
-- becomes, given an @icon-github@ table entry:
--
-- > <span class="icon icon-github" aria-hidden="true">…the SVG…</span>
--
-- An icon written as an autolink is decorative: it is hidden from a screen
-- reader, which is what you want next to text that already says what the
-- link is. Give the link text instead to label it:
--
-- > [GitHub](icon:github)
--
-- > <span class="icon icon-github" role="img" aria-label="GitHub">…the SVG…</span>
--
-- Path components after the name become classes too, so an icon can be
-- given a size or a position by a style sheet of yours:
--
-- > <icon:github/lg>
--
-- > <span class="icon icon-github icon-lg" aria-hidden="true">…the SVG…</span>
--
-- A link that names an icon you do not have is left as it is, so that it is
-- visible in the output rather than missing from it. 'checkIcons' turns it
-- into an error instead.
icons ::
  -- | The icons you have, by name
  Map Text (Html ()) ->
  RenderExtension
icons = iconsWith [scheme|icon|] "icon"

-- | Like 'icons', but you choose the scheme that marks an icon and the
-- prefix of the classes. Documents written for the @fontAwesome@ extension
-- keep working with
--
-- > iconsWith [scheme|fa|] "icon" myIcons
iconsWith ::
  -- | Scheme that marks a link as an icon
  RText 'Scheme ->
  -- | Prefix of the classes to give the icon
  Text ->
  -- | The icons you have, by name
  Map Text (Html ()) ->
  RenderExtension
iconsWith scm prefix table = Render.inlineRender $ \old inline ->
  case inline of
    Link _ inner uri _
      | hasScheme scm uri,
        Just (name, mods) <- iconPath uri,
        Just svg <- M.lookup name table ->
          span_ (class_ (classes name mods) : how inner uri) svg
    other -> old other
  where
    classes name mods = T.unwords (prefix : fmap dashed (name : mods))
    dashed x = prefix <> "-" <> x
    -- An autolink is a link whose text is its own URI, and it is the way to
    -- ask for an icon that says nothing.
    how inner uri =
      let label = Render.asPlainText inner
       in if label == URI.render uri
            then [makeAttribute "aria-hidden" "true"]
            else [makeAttribute "role" "img", makeAttribute "aria-label" label]

-- | Report every link with the @icon@ scheme that does not name one of the
-- icons you have. 'icons' cannot do this itself: it runs while the document
-- is rendered, and by then there is nothing left to report against.
--
-- > MMark.runTrans (checkIcons myIcons) doc
--
-- Only the names matter here, so the table you render with will do.
checkIcons ::
  -- | The icons you have, by name
  Map Text a ->
  Bni ->
  Trans Bni
checkIcons = checkIconsWith [scheme|icon|]

-- | Like 'checkIcons', but you choose the scheme, as in 'iconsWith'.
checkIconsWith ::
  -- | Scheme that marks a link as an icon
  RText 'Scheme ->
  -- | The icons you have, by name
  Map Text a ->
  Bni ->
  Trans Bni
checkIconsWith scm table block = do
  mapM_ check (iconLinks block)
  return block
  where
    iconLinks = foldMap ofInline . inlinesOf
    ofInline = \case
      Link spn _ uri _ | hasScheme scm uri -> [(spn, uri)]
      _ -> []
    check (spn, uri) = case iconPath uri of
      Nothing -> Trans.report spn "this link names no icon"
      Just (name, _)
        | M.member name table -> return ()
        | otherwise ->
            Trans.report spn ("there is no icon called \"" <> name <> "\"")

----------------------------------------------------------------------------
-- Helpers

-- | Whether a URI is written in the given scheme.
hasScheme :: RText 'Scheme -> URI -> Bool
hasScheme scm uri = URI.uriScheme uri == Just scm

-- | The icon a URI names and the modifiers that follow it.
iconPath :: URI -> Maybe (Text, [Text])
iconPath uri = case URI.uriPath uri of
  Just (_, name :| mods) -> Just (URI.unRText name, URI.unRText <$> mods)
  Nothing -> Nothing
