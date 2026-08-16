{-# LANGUAGE OverloadedStrings #-}

module Text.MMark.Extension.ImageSpec (spec) where

import Data.Bits (shiftR, (.&.))
import Data.ByteString (ByteString)
import Data.ByteString qualified as B
import Data.ByteString.Char8 qualified as B8
import Data.Map.Strict qualified as M
import Data.Text (Text)
import Data.Word (Word8)
import System.FilePath ((</>))
import Test.Hspec
import Text.MMark qualified as MMark
import Text.MMark.Extension.Image
import Text.MMark.Extension.TestUtils
import Text.MMark.Trans (Span)
import Text.URI qualified as URI

spec :: Spec
spec = do
  describe "checkAltText" $ do
    it "reports an image with no description" $
      transErrors checkAltText "![](/a.png)"
        `shouldReturn` ["1:1: this image has no description for the alt attribute"]
    it "accepts an image with a description" $
      transErrors checkAltText "![a cat](/a.png)" `shouldReturn` []
    it "reports an image nested in a link" $
      transErrors checkAltText "[![](/a.png)](/x)"
        `shouldReturn` ["1:2: this image has no description for the alt attribute"]
    it "finds an image inside a block quote" $
      transErrors checkAltText "> ![](/a.png)"
        `shouldReturn` ["1:3: this image has no description for the alt attribute"]
    it "reports every undescribed image, once each" $
      transErrors checkAltText "![](/a.png) ![b](/b.png) ![](/c.png)"
        `shouldReturn` [ "1:1: this image has no description for the alt attribute",
                         "1:26: this image has no description for the alt attribute"
                       ]

  describe "lazyImages" $ do
    it "adds the loading and decoding attributes" $
      withExt
        lazyImages
        "![a cat](/a.png)"
        "<p><img loading=\"lazy\" decoding=\"async\" alt=\"a cat\" src=\"/a.png\"></p>\n"
    it "leaves other inlines alone" $
      withExt lazyImages "[a link](/x)" "<p><a href=\"/x\">a link</a></p>\n"

  describe "imageScanner" $ do
    it "collects the URI of an image" $
      scanned "![a cat](/a.png)" `shouldBe` ["/a.png"]
    it "collects every image of a document" $
      scanned "![a](/a.png)\n\n![b](/b.png)" `shouldBe` ["/a.png", "/b.png"]
    it "collects an image nested in a link and in a quote" $
      scanned "[![a](/a.png)](/x)\n\n> ![b](/b.png)"
        `shouldBe` ["/a.png", "/b.png"]
    it "keeps two images with the same URI apart" $
      length (M.toList (scan "![a](/a.png) ![a](/a.png)")) `shouldBe` 2
    it "collects nothing from a document with no images" $
      scanned "just some text" `shouldBe` []

  describe "imageDimensions" $ do
    it "gives an image the size it was measured to have" $
      withSizes
        (Just (640, 480))
        "![a cat](/a.png)"
        "<p><img width=\"640\" height=\"480\" alt=\"a cat\" src=\"/a.png\"></p>\n"
    it "leaves an image that could not be measured alone" $
      withSizes
        Nothing
        "![a cat](/a.png)"
        "<p><img alt=\"a cat\" src=\"/a.png\"></p>\n"
    it "leaves an image with no measurement at all alone" $
      withExt
        (imageDimensions M.empty)
        "![a cat](/a.png)"
        "<p><img alt=\"a cat\" src=\"/a.png\"></p>\n"
    it "composes with lazyImages" $
      withSizesUsing
        (lazyImages <>)
        (Just (7, 3))
        "![a cat](/a.png)"
        "<p><img loading=\"lazy\" decoding=\"async\" width=\"7\" height=\"3\" alt=\"a cat\" src=\"/a.png\"></p>\n"

  describe "imageSizeOf" $ do
    it "measures a PNG" $
      measuring (pngBytes 7 3) `shouldReturn` Just (7, 3)
    it "measures a PNG larger than a byte in each direction" $
      measuring (pngBytes 1920 1080) `shouldReturn` Just (1920, 1080)
    it "measures a GIF" $
      measuring (gifBytes 11 5) `shouldReturn` Just (11, 5)
    it "measures a GIF larger than a byte in each direction" $
      measuring (gifBytes 800 600) `shouldReturn` Just (800, 600)
    it "measures a JPEG" $
      measuring (jpegBytes [] 13 9) `shouldReturn` Just (13, 9)
    it "measures a JPEG behind a segment it does not care about" $
      measuring (jpegBytes [app0, comment 40] 320 240)
        `shouldReturn` Just (320, 240)
    it "measures a JPEG behind a marker that carries no payload" $
      -- 0xD8 is SOI, whose two following bytes are not a length; a walk
      -- that reads them as one lands in the middle of nothing.
      measuring (jpegBytes [app0, standalone 0xD8, comment 8] 64 48)
        `shouldReturn` Just (64, 48)
    it "measures a JPEG whose frame is not the baseline one" $
      -- SOF2, the progressive frame header
      measuring (jpegBytesWith 0xC2 [app0] 21 12) `shouldReturn` Just (21, 12)
    it "does not mistake a huffman table for a frame" $
      -- 0xC4 is in the SOF range by number but is not a frame
      measuring (jpegBytes [tableNotAFrame] 30 20) `shouldReturn` Just (30, 20)
    it "gives up on a JPEG that ends before its frame" $
      measuring (B.pack [0xFF, 0xD8] <> app0) `shouldReturn` Nothing
    it "gives up on a JPEG whose segment lengths are nonsense" $
      -- a segment that claims to be no bytes long, then one that claims to
      -- run past the end of the file
      measuring
        ( B.pack [0xFF, 0xD8, 0xFF, 0xE0, 0x00, 0x00]
            <> B.replicate 8 0x20
            <> B.pack [0xFF, 0xC0]
            <> be16 11
            <> B.pack [8]
            <> be16 99
            <> be16 99
            <> B.pack [1, 1, 0x11, 0]
        )
        `shouldReturn` Nothing
    it "gives up on a file that is not an image" $
      measuring (B8.pack "just some text, not an image at all")
        `shouldReturn` Nothing
    it "gives up on an empty file" $
      measuring B.empty `shouldReturn` Nothing
    it "gives up on a truncated PNG" $
      measuring (B.take 20 (pngBytes 7 3)) `shouldReturn` Nothing
    it "gives up on a truncated GIF" $
      measuring (B.take 8 (gifBytes 11 5)) `shouldReturn` Nothing
    it "gives up on a file that is not there instead of throwing" $
      withTempDir (\dir -> imageSizeOf (dir </> "nope.png"))
        `shouldReturn` Nothing
    it "gives up on a directory instead of throwing" $
      withTempDir imageSizeOf `shouldReturn` Nothing

----------------------------------------------------------------------------
-- Helpers

-- | Scan a document and return the URI of every image it has, in order.
scanned :: Text -> [Text]
scanned = fmap URI.render . M.elems . scan

scan :: Text -> M.Map Span URI.URI
scan input = case MMark.parse "" input of
  Left _ -> error "the test input does not parse"
  Right doc -> MMark.runScanner imageScanner doc

-- | Render a document with every image measured as the given size.
withSizes :: Maybe (Int, Int) -> Text -> Text -> Expectation
withSizes = withSizesUsing id

withSizesUsing ::
  -- | What else to render with
  (MMark.RenderExtension -> MMark.RenderExtension) ->
  -- | The size every image is measured to have
  Maybe (Int, Int) ->
  -- | Input for the parser
  Text ->
  -- | Expected output of the render
  Text ->
  Expectation
withSizesUsing f size input expected =
  withExt (f (imageDimensions (size <$ scan input))) input expected

-- | The bytes of a PNG of the given size: the signature and the @IHDR@
-- chunk, which is all that states the size.
pngBytes :: Int -> Int -> ByteString
pngBytes w h =
  B.pack [137, 80, 78, 71, 13, 10, 26, 10]
    <> be32 13
    <> B8.pack "IHDR"
    <> be32 w
    <> be32 h
    <> B.pack [8, 2, 0, 0, 0]

-- | The bytes of a GIF of the given size: the signature and the logical
-- screen descriptor.
gifBytes :: Int -> Int -> ByteString
gifBytes w h = B8.pack "GIF89a" <> le16 w <> le16 h <> B.pack [0, 0, 0]

-- | The bytes of a JPEG of the given size: @SOI@, the given segments, then
-- a baseline frame header.
jpegBytes :: [ByteString] -> Int -> Int -> ByteString
jpegBytes = jpegBytesWith 0xC0

-- | Like 'jpegBytes', but you choose which frame header states the size.
jpegBytesWith :: Word8 -> [ByteString] -> Int -> Int -> ByteString
jpegBytesWith marker leading w h =
  B.pack [0xFF, 0xD8] <> B.concat leading <> sof <> B.pack [0xFF, 0xD9]
  where
    sof =
      B.pack [0xFF, marker]
        <> be16 11
        <> B.pack [8]
        <> be16 h
        <> be16 w
        <> B.pack [1, 1, 0x11, 0]

-- | A @JFIF@ header, the segment that usually comes first.
app0 :: ByteString
app0 =
  B.pack [0xFF, 0xE0]
    <> be16 16
    <> B8.pack "JFIF\NUL"
    <> B.pack [1, 1, 0, 0, 1, 0, 1, 0, 0]

-- | A comment segment carrying the given number of bytes of padding.
comment :: Int -> ByteString
comment n = B.pack [0xFF, 0xFE] <> be16 (n + 2) <> B.replicate n 0x20

-- | A marker that carries no payload at all.
standalone :: Word8 -> ByteString
standalone m = B.pack [0xFF, m]

-- | A huffman table, which sits in the range the frame headers occupy but
-- is not one of them.
tableNotAFrame :: ByteString
tableNotAFrame = B.pack [0xFF, 0xC4] <> be16 6 <> B.replicate 4 0

-- | Write the given bytes to a file and measure it.
measuring :: ByteString -> IO (Maybe (Int, Int))
measuring bs = withTempDir $ \dir -> do
  let path = dir </> "image"
  B.writeFile path bs
  imageSizeOf path

be32 :: Int -> ByteString
be32 n = B.pack (fmap (byte n) [24, 16, 8, 0])

be16 :: Int -> ByteString
be16 n = B.pack (fmap (byte n) [8, 0])

le16 :: Int -> ByteString
le16 n = B.pack (fmap (byte n) [0, 8])

byte :: Int -> Int -> Word8
byte n s = fromIntegral ((n `shiftR` s) .&. 0xFF)
