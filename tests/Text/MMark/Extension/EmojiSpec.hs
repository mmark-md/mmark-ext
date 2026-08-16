{-# LANGUAGE OverloadedStrings #-}

module Text.MMark.Extension.EmojiSpec (spec) where

import Data.Map.Strict qualified as M
import Test.Hspec
import Text.MMark.Extension.Emoji
import Text.MMark.Extension.TestUtils

spec :: Spec
spec = do
  emojiSpec
  emojiWithSpec

emojiSpec :: Spec
emojiSpec = describe "emoji" $ do
  it "replaces a shortcode it knows" $
    withTrans emoji "Hi :smile: there" "<p>Hi \128578 there</p>\n"
  it "replaces several in one go" $
    withTrans emoji ":fire: :rocket:" "<p>\128293 \128640</p>\n"
  it "replaces one that is more than one code point" $
    withTrans emoji ":warning:" "<p>\9888\65039</p>\n"
  it "replaces a shortcode that is an alias of another" $
    withTrans emoji ":joy: :laughing:" "<p>\128514 \128514</p>\n"
  it "replaces one whose name is not letters" $
    withTrans emoji ":+1: :100:" "<p>\128077 \128175</p>\n"
  it "reports a shortcode it does not know" $
    transErrors emoji "Hi :nosuch: there"
      `shouldReturn` ["1:1: there is no emoji called \"nosuch\""]
  it "leaves a lone colon alone" $
    withTrans emoji "at 12:30 sharp" "<p>at 12:30 sharp</p>\n"
  it "leaves text with no colons alone" $
    withTrans emoji "nothing here" "<p>nothing here</p>\n"
  it "leaves a shortcode in a code span alone" $
    withTrans emoji "`:smile:`" "<p><code>:smile:</code></p>\n"
  it "replaces a shortcode nested in other markup" $
    withTrans emoji "**:fire:**" "<p><strong>\128293</strong></p>\n"
  it "reports every unknown shortcode, not just the first" $
    transErrors emoji ":nosuch: and :neither:"
      `shouldReturn` [ "1:1: there is no emoji called \"nosuch\"",
                       "1:1: there is no emoji called \"neither\""
                     ]

emojiWithSpec :: Spec
emojiWithSpec = describe "emojiWith" $ do
  it "uses the table it is given" $
    withTrans (emojiWith table) "look :cat:" "<p>look \128049</p>\n"
  it "reports a shortcode the table does not have" $
    transErrors (emojiWith table) ":smile:"
      `shouldReturn` ["1:1: there is no emoji called \"smile\""]
  it "reports nothing for a table that has everything" $
    transErrors (emojiWith table) ":cat: :dog:" `shouldReturn` []
  it "reports every unknown shortcode of a paragraph" $
    transErrors (emojiWith table) ":nope: and :also:"
      `shouldReturn` [ "1:1: there is no emoji called \"nope\"",
                       "1:1: there is no emoji called \"also\""
                     ]
  it "replaces nothing when the table is empty" $
    withTrans (emojiWith mempty) "no colons here" "<p>no colons here</p>\n"
  where
    table = M.fromList [("cat", "\128049"), ("dog", "\128054")]
