{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      :  Text.MMark.Extension.Emoji
-- Copyright   :  © 2026–present Mark Karpov
-- License     :  BSD 3 clause
--
-- Maintainer  :  Mark Karpov <markkarpov92@gmail.com>
-- Stability   :  experimental
-- Portability :  portable
--
-- Replace @:shortcode:@ with the emoji it names.
--
-- @since 0.3.0.0
module Text.MMark.Extension.Emoji
  ( emoji,
    emojiWith,
    defaultEmoji,
  )
where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.Text (Text)
import Data.Text qualified as T
import Text.MMark.Trans (Bni, Inline (..), Trans)
import Text.MMark.Trans qualified as Trans

-- | Replace every @:shortcode:@ of 'defaultEmoji' with the emoji it names,
-- and report every @:shortcode:@ that is not one of them. A name that is
-- not recognized is far more likely to be a typo than something the writer
-- meant to keep.
emoji :: Bni -> Trans Bni
emoji = emojiWith defaultEmoji

-- | Like 'emoji', but you supply the table.
emojiWith :: Map Text Text -> Bni -> Trans Bni
emojiWith table = Trans.bottomUpInlines $ \case
  Plain spn txt -> Plain spn <$> replace spn txt
  other -> return other
  where
    replace spn = fmap T.concat . mapM (piece spn) . chunks
    piece spn = \case
      Left t -> return t
      Right name -> case M.lookup name table of
        Just e -> return e
        Nothing -> do
          Trans.report spn ("there is no emoji called \"" <> name <> "\"")
          return (":" <> name <> ":")

-- | Split text into literal pieces and the shortcodes between them. A
-- shortcode is a run of letters, digits, @_@, @+@, and @-@ between colons.
chunks :: Text -> [Either Text Text]
chunks t =
  case T.breakOn ":" t of
    (before, rest)
      | T.null rest -> [Left before | not (T.null before)]
      | otherwise ->
          let (name, rest') = T.breakOn ":" (T.drop 1 rest)
           in if T.null rest' || T.null name || not (T.all nameChar name)
                then case chunks (T.drop 1 rest) of
                  cs -> Left (before <> ":") : cs
                else Left before : Right name : chunks (T.drop 1 rest')
  where
    nameChar c = c `elem` ("_+-" :: String) || c `elem` ['a' .. 'z'] || c `elem` ['0' .. '9']

-- | The table 'emoji' uses: a couple of hundred of the shortcodes that come
-- up most often, grouped below by what they are about. The names are the
-- familiar ones, so @:tada:@, @:+1:@, and @:warning:@ mean what you expect.
defaultEmoji :: Map Text Text
defaultEmoji =
  M.fromList
    [ -- Faces and emotions
      ("smile", "\128578"),
      ("grin", "\128512"),
      ("grinning", "\128512"),
      ("laughing", "\128514"),
      ("joy", "\128514"),
      ("sweat_smile", "\128517"),
      ("rofl", "\129315"),
      ("wink", "\128521"),
      ("blush", "\128522"),
      ("heart_eyes", "\128525"),
      ("star_struck", "\129321"),
      ("sunglasses", "\128526"),
      ("smirk", "\128527"),
      ("stuck_out_tongue", "\128539"),
      ("nerd_face", "\129299"),
      ("thinking", "\129300"),
      ("zipper_mouth_face", "\129296"),
      ("face_with_monocle", "\129488"),
      ("neutral_face", "\128528"),
      ("confused", "\128533"),
      ("upside_down_face", "\128579"),
      ("worried", "\128543"),
      ("cry", "\128546"),
      ("sob", "\128557"),
      ("tired_face", "\128555"),
      ("scream", "\128561"),
      ("angry", "\128544"),
      ("rage", "\128545"),
      ("sleeping", "\128564"),
      ("yawning_face", "\129393"),
      ("exploding_head", "\129327"),
      ("partying_face", "\129395"),
      ("shrug", "\129335"),
      ("facepalm", "\129318"),
      -- Hands
      ("thumbsup", "\128077"),
      ("thumbsdown", "\128078"),
      ("+1", "\128077"),
      ("-1", "\128078"),
      ("ok_hand", "\128076"),
      ("v", "\9996\65039"),
      ("point_up", "\9757\65039"),
      ("point_right", "\128073"),
      ("point_left", "\128072"),
      ("wave", "\128075"),
      ("clap", "\128079"),
      ("raised_hands", "\128588"),
      ("pray", "\128591"),
      ("handshake", "\129309"),
      ("muscle", "\128170"),
      ("writing_hand", "\9997\65039"),
      ("eyes", "\128064"),
      -- Hearts
      ("heart", "\10084\65039"),
      ("broken_heart", "\128148"),
      ("sparkling_heart", "\128150"),
      ("blue_heart", "\128153"),
      ("green_heart", "\128154"),
      ("yellow_heart", "\128155"),
      ("orange_heart", "\129505"),
      ("purple_heart", "\128156"),
      ("black_heart", "\128420"),
      -- Nature and weather
      ("sunny", "\9728\65039"),
      ("crescent_moon", "\127769"),
      ("star", "\11088"),
      ("sparkles", "\10024"),
      ("cloud", "\9729\65039"),
      ("zap", "\9889"),
      ("snowflake", "\10052\65039"),
      ("rainbow", "\127752"),
      ("droplet", "\128167"),
      ("ocean", "\127754"),
      ("earth_americas", "\127758"),
      ("mountain", "\9968\65039"),
      ("seedling", "\127793"),
      ("herb", "\127807"),
      ("four_leaf_clover", "\127808"),
      ("maple_leaf", "\127809"),
      ("cactus", "\127797"),
      ("palm_tree", "\127796"),
      ("fire", "\128293"),
      -- Animals
      ("snail", "\128012"),
      ("turtle", "\128034"),
      ("rabbit", "\128007"),
      ("cat", "\128049"),
      ("dog", "\128054"),
      ("mouse", "\128045"),
      ("horse", "\128052"),
      ("pig", "\128055"),
      ("bear", "\128059"),
      ("panda_face", "\128060"),
      ("fox_face", "\129418"),
      ("monkey", "\128018"),
      ("elephant", "\128024"),
      ("camel", "\128043"),
      ("unicorn", "\129412"),
      ("dragon", "\128009"),
      ("snake", "\128013"),
      ("bird", "\128038"),
      ("owl", "\129417"),
      ("penguin", "\128039"),
      ("fish", "\128031"),
      ("whale", "\128051"),
      ("octopus", "\128025"),
      ("crab", "\129408"),
      ("bug", "\128027"),
      ("bee", "\128029"),
      ("ant", "\128028"),
      ("butterfly", "\129419"),
      -- Food and drink
      ("coffee", "\9749"),
      ("tea", "\127861"),
      ("beer", "\127866"),
      ("wine_glass", "\127863"),
      ("cocktail", "\127864"),
      ("champagne", "\127870"),
      ("clinking_glasses", "\129346"),
      ("pizza", "\127829"),
      ("hamburger", "\127828"),
      ("taco", "\127790"),
      ("sushi", "\127843"),
      ("popcorn", "\127871"),
      ("cake", "\127856"),
      ("birthday", "\127874"),
      ("cookie", "\127850"),
      ("doughnut", "\127849"),
      ("ice_cream", "\127848"),
      ("chocolate_bar", "\127851"),
      ("apple", "\127822"),
      ("banana", "\127820"),
      ("avocado", "\129361"),
      -- Tools and objects
      ("bulb", "\128161"),
      ("wrench", "\128295"),
      ("hammer", "\128296"),
      ("hammer_and_wrench", "\128736\65039"),
      ("nut_and_bolt", "\128297"),
      ("gear", "\9881\65039"),
      ("toolbox", "\129520"),
      ("microscope", "\128300"),
      ("telescope", "\128301"),
      ("mag", "\128269"),
      ("computer", "\128187"),
      ("keyboard", "\9000\65039"),
      ("floppy_disk", "\128190"),
      ("package", "\128230"),
      ("battery", "\128267"),
      ("electric_plug", "\128268"),
      ("camera", "\128247"),
      ("movie_camera", "\127909"),
      ("tv", "\128250"),
      ("bell", "\128276"),
      ("mega", "\128227"),
      ("loudspeaker", "\128226"),
      ("speech_balloon", "\128172"),
      ("thought_balloon", "\128173"),
      ("envelope", "\9993\65039"),
      ("inbox_tray", "\128229"),
      ("outbox_tray", "\128228"),
      ("flashlight", "\128294"),
      ("candle", "\128367\65039"),
      ("broom", "\129529"),
      ("wastebasket", "\128465\65039"),
      ("crystal_ball", "\128302"),
      ("gem", "\128142"),
      ("crown", "\128081"),
      ("trophy", "\127942"),
      ("dart", "\127919"),
      ("game_die", "\127922"),
      ("art", "\127912"),
      ("musical_note", "\127925"),
      ("rocket", "\128640"),
      ("airplane", "\9992\65039"),
      ("hourglass", "\8987"),
      ("alarm_clock", "\9200"),
      ("stopwatch", "\9201\65039"),
      ("calendar", "\128197"),
      ("balance_scale", "\9878\65039"),
      ("chart_with_upwards_trend", "\128200"),
      ("bar_chart", "\128202"),
      -- Paper and files
      ("book", "\128214"),
      ("books", "\128218"),
      ("memo", "\128221"),
      ("pencil2", "\9999\65039"),
      ("scroll", "\128220"),
      ("page_facing_up", "\128196"),
      ("newspaper", "\128240"),
      ("clipboard", "\128203"),
      ("file_folder", "\128193"),
      ("open_file_folder", "\128194"),
      ("paperclip", "\128206"),
      ("pushpin", "\128204"),
      ("bookmark", "\128278"),
      ("label", "\127991\65039"),
      ("link", "\128279"),
      ("lock", "\128274"),
      ("unlock", "\128275"),
      ("closed_lock_with_key", "\128272"),
      ("key", "\128273"),
      ("shield", "\128737\65039"),
      -- Marks and signs
      ("white_check_mark", "\9989"),
      ("heavy_check_mark", "\10004\65039"),
      ("ballot_box_with_check", "\9745\65039"),
      ("x", "\10060"),
      ("question", "\10067"),
      ("exclamation", "\10071"),
      ("warning", "\9888\65039"),
      ("boom", "\128165"),
      ("100", "\128175"),
      ("tada", "\127881"),
      ("checkered_flag", "\127937"),
      ("triangular_flag_on_post", "\128681"),
      ("construction", "\128679"),
      ("rotating_light", "\128680"),
      ("no_entry", "\9940"),
      ("recycle", "\9851\65039"),
      ("infinity", "\9854\65039"),
      ("heavy_plus_sign", "\10133"),
      ("heavy_minus_sign", "\10134"),
      ("arrow_right", "\10145\65039"),
      ("arrow_left", "\11013\65039"),
      ("arrow_up", "\11014\65039"),
      ("arrow_down", "\11015\65039"),
      ("arrows_counterclockwise", "\128260"),
      ("red_circle", "\128308"),
      ("large_blue_circle", "\128309"),
      ("green_circle", "\128994"),
      ("yellow_circle", "\128993"),
      ("orange_circle", "\128992"),
      ("purple_circle", "\128995"),
      ("white_circle", "\9898"),
      ("black_circle", "\9899"),
      -- Other
      ("ghost", "\128123"),
      ("alien", "\128125"),
      ("robot", "\129302"),
      ("skull", "\128128"),
      ("zzz", "\128164")
    ]
