{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      :  Text.MMark.Extension.Image
-- Copyright   :  © 2026–present Mark Karpov
-- License     :  BSD 3 clause
--
-- Maintainer  :  Mark Karpov <markkarpov92@gmail.com>
-- Stability   :  experimental
-- Portability :  portable
--
-- Tell the browser how large an image is before it has been fetched, let it
-- decide when to fetch it, and say when an image describes itself to nobody.
--
-- An @\<img\>@ without @width@ and @height@ makes the page move under the
-- reader while the image loads, which is the layout shift every measure of
-- page quality penalizes.
--
-- 'lazyImages' and 'checkAltText' need nothing but the document. The width
-- and height have to be measured first, which 'imageScanner',
-- 'imageSizeOf', and 'imageDimensions' do between them.
--
-- @since 0.3.0.0
module Text.MMark.Extension.Image
  ( lazyImages,
    checkAltText,
    imageScanner,
    imageDimensions,
    imageSizeOf,
  )
where

import Control.Exception (IOException, try)
import Control.Foldl qualified as L
import Data.Bits (shiftL, (.|.))
import Data.ByteString qualified as B
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.Text qualified as T
import Data.Word (Word8)
import Lucid
import Lucid.Base (makeAttribute)
import System.IO (IOMode (..), withBinaryFile)
import Text.MMark qualified as MMark
import Text.MMark.Extension.Internal (inlinesOf)
import Text.MMark.Render (Bni, Inline (..), RenderExtension, Span)
import Text.MMark.Render qualified as Render
import Text.MMark.Trans (Trans)
import Text.MMark.Trans qualified as Trans
import Text.URI (URI)

-- | Give every image @loading=\"lazy\"@ and @decoding=\"async\"@, so that
-- an image far down the page does not hold up the ones the reader can see.
lazyImages :: RenderExtension
lazyImages = Render.inlineRender $ \old inline ->
  case inline of
    i@Image {} ->
      with
        (old i)
        [ makeAttribute "loading" "lazy",
          makeAttribute "decoding" "async"
        ]
    other -> old other

-- | Report every image whose description is empty. A reader who cannot see
-- the image is told nothing about it, and a search engine cannot index it.
--
-- Note that MMark renders such an image as @\<img alt src=\"…\"\>@ without
-- complaining, so nothing else in the pipeline will tell you.
checkAltText :: Bni -> Trans Bni
checkAltText block = do
  mapM_ check (inlinesOf block)
  return block
  where
    check = \case
      Image spn desc _ _
        | Trans.asPlainText desc == "" ->
            Trans.report spn "this image has no description for the alt attribute"
      _ -> return ()

-- | Collect the URI of every image of a document, by the span of the image
-- it belongs to.
--
-- > let imgs = MMark.runScanner imageScanner doc
-- > sizes <- traverse (imageSizeOf . toPath) imgs
-- > TL.putStr (renderText (MMark.render (imageDimensions sizes) doc))
imageScanner :: L.Fold Bni (Map Span URI)
imageScanner = MMark.scanner M.empty $ \acc block ->
  foldr insert acc (inlinesOf block)
  where
    insert = \case
      Image spn _ uri _ -> M.insert spn uri
      _ -> id

-- | Give each image the width and height it was measured to have. An image
-- with no measurement, or one that could not be measured, is left alone.
imageDimensions :: Map Span (Maybe (Int, Int)) -> RenderExtension
imageDimensions sizes = Render.inlineRender $ \old inline ->
  case inline of
    i@(Image spn _ _ _) ->
      case M.lookup spn sizes of
        Just (Just (w, h)) ->
          with (old i) [width_ (tshow w), height_ (tshow h)]
        _ -> old i
    other -> old other
  where
    tshow = T.pack . show

-- | Measure a PNG, GIF, or JPEG file without decoding it, by reading the
-- header that states its size. Anything else gives 'Nothing'.
imageSizeOf :: FilePath -> IO (Maybe (Int, Int))
imageSizeOf path = do
  r <- try (withBinaryFile path ReadMode (`B.hGet` headerLimit))
  return $ case r of
    Left (_ :: IOException) -> Nothing
    Right bs -> sizeOfPng bs `orElse` sizeOfGif bs `orElse` sizeOfJpeg bs
  where
    orElse (Just x) _ = Just x
    orElse Nothing y = y

-- | How much of a file 'imageSizeOf' reads looking for the header that
-- states its size.
headerLimit :: Int
headerLimit = 256 * 1024

-- | @IHDR@ holds the size in the first two big-endian words of its data.
sizeOfPng :: B.ByteString -> Maybe (Int, Int)
sizeOfPng bs
  | B.take 8 bs == B.pack [137, 80, 78, 71, 13, 10, 26, 10],
    B.length bs >= 24 =
      Just (be32 (B.drop 16 bs), be32 (B.drop 20 bs))
  | otherwise = Nothing

-- | The logical screen descriptor holds the size in little-endian shorts.
sizeOfGif :: B.ByteString -> Maybe (Int, Int)
sizeOfGif bs
  | B.take 3 bs == "GIF",
    B.length bs >= 10 =
      Just (le16 (B.drop 6 bs), le16 (B.drop 8 bs))
  | otherwise = Nothing

-- | Walk the segments of a JPEG until one of the frame headers, which
-- carries the size after a byte of precision.
sizeOfJpeg :: B.ByteString -> Maybe (Int, Int)
sizeOfJpeg bs
  | B.take 2 bs == B.pack [0xFF, 0xD8] = go (B.drop 2 bs)
  | otherwise = Nothing
  where
    go s = do
      (marker, rest) <- segment s
      if isFrame marker
        then
          if B.length rest >= 7
            then Just (be16 (B.drop 5 rest), be16 (B.drop 3 rest))
            else Nothing
        else
          if isStandalone marker
            then if marker == 0xD9 then Nothing else go rest
            else
              if B.length rest >= 2
                then go (B.drop (be16 rest) rest)
                else Nothing
    segment s =
      let s' = B.dropWhile (== 0xFF) s
       in if B.null s' then Nothing else Just (B.head s', B.drop 1 s')
    -- SOF0 through SOF15, less the four markers that are not frames
    isFrame m =
      m >= 0xC0 && m <= 0xCF && m /= 0xC4 && m /= 0xC8 && m /= 0xCC

-- | Whether a JPEG marker carries no payload, in which case the two bytes
-- that follow it are not a length: TEM, the eight restart markers, SOI, and
-- EOI.
isStandalone :: Word8 -> Bool
isStandalone m = m == 0x01 || (m >= 0xD0 && m <= 0xD9)

be32 :: B.ByteString -> Int
be32 b =
  (fromIntegral (B.index b 0) `shiftL` 24)
    .|. (fromIntegral (B.index b 1) `shiftL` 16)
    .|. (fromIntegral (B.index b 2) `shiftL` 8)
    .|. fromIntegral (B.index b 3)

be16 :: B.ByteString -> Int
be16 b = (fromIntegral (B.index b 0) `shiftL` 8) .|. fromIntegral (B.index b 1)

le16 :: B.ByteString -> Int
le16 b = (fromIntegral (B.index b 1) `shiftL` 8) .|. fromIntegral (B.index b 0)
