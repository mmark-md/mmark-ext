{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

-- |
-- Module      :  Text.MMark.Extension.Link
-- Copyright   :  © 2018–present Mark Karpov
-- License     :  BSD 3 clause
--
-- Maintainer  :  Mark Karpov <markkarpov92@gmail.com>
-- Stability   :  experimental
-- Portability :  portable
--
-- Say where a link opens, and find the links that lead nowhere.
--
-- 'linkTarget' is the only render extension here; the rest are checks. The
-- three checks cost increasingly more, so they are separate: checking
-- fragments needs nothing but the document, checking local files needs the
-- file system, and checking the rest needs whatever you are willing to do
-- to find out.
--
-- @since 0.3.0.0
module Text.MMark.Extension.Link
  ( linkTarget,
    headerIdScanner,
    checkFragments,
    checkLocalFiles,
    checkExternal,
  )
where

import Control.Foldl qualified as L
import Control.Monad.IO.Class (liftIO)
import Data.Foldable (asum)
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import Data.Set qualified as S
import Data.Text qualified as T
import Lucid
import System.Directory (doesDirectoryExist, doesFileExist)
import System.FilePath ((</>))
import Text.MMark qualified as MMark
import Text.MMark.Extension.Internal (inlinesOf)
import Text.MMark.Render (RenderExtension)
import Text.MMark.Render qualified as Render
import Text.MMark.Trans (Block (..), Bni, Inline (..), Trans, TransT)
import Text.MMark.Trans qualified as Trans
import Text.URI (URI (..))
import Text.URI qualified as URI

-- | When the title of a link starts with the word @\"_blank\"@,
-- @\"_self\"@, @\"_parent\"@, or @\"_top\"@, it's stripped from the title (as
-- well as all whitespace after it) and added as the value of the @target@
-- attribute of the resulting link.
--
-- For example:
--
-- > This [link](/url '_blank My title') opens in new tab.
--
-- A link that opens in a new browsing context also gets
-- @rel=\"noopener noreferrer\"@. Without it the page that is opened can
-- reach back to the page that opened it through @window.opener@, and the
-- referrer is disclosed to it.
linkTarget :: RenderExtension
linkTarget = Render.inlineRender $ \old inline ->
  case inline of
    l@(Link spn txt url (Just title)) -> fromMaybe (old l) $ do
      let f prefix =
            (prefix,) . T.stripStart
              <$> T.stripPrefix prefix title
      (prefix, title') <-
        asum $
          f <$> ["_blank", "_self", "_parent", "_top"]
      let mtitle = if T.null title' then Nothing else Just title'
          -- Only a new browsing context can reach back through
          -- window.opener, so the other targets do not need protecting.
          relAttrs =
            [rel_ "noopener noreferrer" | prefix == "_blank"]
      return $
        with (old (Link spn txt url mtitle)) (target_ prefix : relAttrs)
    other -> old other

-- | Collect the ids MMark gives to the headings of a document, so that
-- 'checkFragments' can tell whether a link into the document leads
-- anywhere.
headerIdScanner :: L.Fold Bni (Set T.Text)
headerIdScanner = MMark.scanner S.empty $ \acc block ->
  case block of
    Heading1 _ x -> add x acc
    Heading2 _ x -> add x acc
    Heading3 _ x -> add x acc
    Heading4 _ x -> add x acc
    Heading5 _ x -> add x acc
    Heading6 _ x -> add x acc
    _ -> acc
  where
    add x = S.insert (Trans.headerId x)

-- | Report every link of the form @#section@ whose fragment no heading of
-- the document defines.
--
-- > let ids = MMark.runScanner headerIdScanner doc
-- > MMark.runTrans (checkFragments ids) doc
checkFragments :: Set T.Text -> Bni -> Trans Bni
checkFragments ids block = do
  mapM_ check (links block)
  return block
  where
    check (spn, uri) = case internalFragment uri of
      Just f
        | not (f `S.member` ids) ->
            Trans.report
              spn
              ("no heading of this document has the id \"" <> f <> "\"")
      _ -> return ()

-- | Report every link to a path that does not exist, relative to the given
-- directory. Links with a scheme or an authority are left to
-- 'checkExternal'.
checkLocalFiles :: FilePath -> Bni -> TransT IO Bni
checkLocalFiles base block = do
  mapM_ check (links block)
  return block
  where
    check (spn, uri) = case localPath uri of
      Nothing -> return ()
      Just p -> do
        let path = base </> T.unpack p
        there <- liftIO $ (||) <$> doesFileExist path <*> doesDirectoryExist path
        if there
          then return ()
          else Trans.report spn ("there is nothing at " <> T.pack path)

-- | Report every link the given action says is unreachable. The action is
-- yours to write, so that this package needs no HTTP client of its own and
-- so that you can cache, rate limit, or skip whatever you like.
--
-- > checkExternal (\uri -> (== 200) . statusCode <$> headRequest uri)
checkExternal :: (URI -> IO Bool) -> Bni -> TransT IO Bni
checkExternal reachable block = do
  mapM_ check (links block)
  return block
  where
    check (spn, uri) =
      case (URI.uriScheme uri, localPath uri, internalFragment uri) of
        (Nothing, _, _) -> return ()
        (_, Just _, _) -> return ()
        (_, _, Just _) -> return ()
        _ -> do
          ok <- liftIO (reachable uri)
          if ok
            then return ()
            else Trans.report spn ("cannot reach " <> URI.render uri)

----------------------------------------------------------------------------
-- Helpers

-- | The links and images of a block, with the span to report against.
links :: Bni -> [(Trans.Span, URI)]
links = foldMap ofInline . inlinesOf
  where
    ofInline = \case
      Link spn _ uri _ -> [(spn, uri)]
      Image spn _ uri _ -> [(spn, uri)]
      _ -> []

-- | The fragment of a URI that points into the document it appears in.
internalFragment :: URI -> Maybe T.Text
internalFragment uri =
  case (uriScheme uri, uriAuthority uri, uriPath uri, uriFragment uri) of
    (Nothing, Left False, Nothing, Just f) -> Just (URI.unRText f)
    _ -> Nothing

-- | The path of a URI that points at a file next to the document.
localPath :: URI -> Maybe T.Text
localPath uri =
  case (uriScheme uri, uriAuthority uri, uriPath uri) of
    (Nothing, Left False, Just (_, ps)) ->
      Just (T.intercalate "/" (URI.unRText <$> foldr (:) [] ps))
    _ -> Nothing
