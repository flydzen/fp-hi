{-# OPTIONS_GHC -Wno-deprecations #-}
module HW3.Pretty where
import           Data.ByteString
import           Data.Char                                 (digitToInt,
                                                            intToDigit)
import           Data.Foldable
import qualified Data.Map
import           Data.Ratio                                (denominator,
                                                            numerator, (%))
import           Data.Scientific                           (FPFormat (Fixed, Generic),
                                                            formatScientific,
                                                            fromRationalRepetendUnlimited,
                                                            toRealFloat)
import           Data.Text.Prettyprint.Doc
import           Data.Text.Prettyprint.Doc.Render.Terminal
import           Data.Word                                 (Word8)
import           HW3.Base
import           Numeric                                   (showHex,
                                                            showIntAtBase)
import           Text.Megaparsec

prettyValue :: HiValue -> Doc AnsiStyle
prettyValue (HiValueFunction f) = pretty $ case f of
  HiFunDiv            -> "div"
  HiFunMul            -> "mul"
  HiFunAdd            -> "add"
  HiFunSub            -> "sub"
  HiFunNot            -> "not"
  HiFunAnd            -> "and"
  HiFunOr             -> "or"
  HiFunLessThan       -> "less-than"
  HiFunGreaterThan    -> "greater-than"
  HiFunEquals         -> "equals"
  HiFunNotLessThan    -> "not-less-than"
  HiFunNotGreaterThan -> "not-greater-than"
  HiFunNotEquals      -> "not-equals"
  HiFunIf             -> "if"
  HiFunLength         -> "length"
  HiFunToUpper        -> "to-upper"
  HiFunToLower        -> "to-lower"
  HiFunReverse        -> "reverse"
  HiFunTrim           -> "trim"
  HiFunList           -> "list"
  HiFunRange          -> "range"
  HiFunFold           -> "fold"
  HiFunPackBytes      -> "pack-bytes"
  HiFunUnpackBytes    -> "unpack-bytes"
  HiFunEncodeUtf8     -> "encode-utf8"
  HiFunDecodeUtf8     -> "decode-utf8"
  HiFunZip            -> "zip"
  HiFunUnzip          -> "unzip"
  HiFunSerialise      -> "serialise"
  HiFunDeserialise    -> "deserialise"
  HiFunRead           -> "read"
  HiFunWrite          -> "write"
  HiFunMkDir          -> "mkdir"
  HiFunChDir          -> "cd"
  HiFunParseTime      -> "parse-time"
  HiFunRand           -> "rand"
  HiFunEcho           -> "echo"
  HiFunCount          -> "count"
  HiFunKeys           -> "keys"
  HiFunValues         -> "values"
  HiFunInvert         -> "invert"

prettyValue (HiValueBool x)     = pretty $ if x then "true" else "false"
prettyValue  HiValueNull        = pretty "null"
prettyValue (HiValueString s)   = pretty "\"" <> pretty s <> pretty "\""
prettyValue (HiValueList l)     = list $ toList (fmap prettyValue l)
prettyValue (HiValueBytes b)    = align (encloseSep (pretty "[#") (pretty " #]") (pretty "")
                                    (Prelude.map prettyByte $ unpack b))
prettyValue (HiValueAction ac)  = case ac of
  HiActionRead fp     -> cat $ Prelude.map pretty ["read(\"", fp, "\")"]
  HiActionWrite fp dt -> cat (Prelude.map pretty ["write(\"", fp, "\", "]) <> prettyValue (HiValueBytes dt) <> pretty ")"
  HiActionMkDir fp    -> cat $ Prelude.map pretty ["mkdir(\"", fp, "\")"]
  HiActionChDir fp    -> cat $ Prelude.map pretty ["cd(\"", fp, "\")"]
  HiActionCwd         -> cat $ Prelude.map pretty ["cwd"]
  HiActionNow         -> cat $ Prelude.map pretty ["now"]
  HiActionRand l r    -> cat $ Prelude.map pretty ["rand(", show l, ", ", show r, ")"]
  HiActionEcho s      -> cat [pretty "echo(\"", pretty s, pretty "\")"]

prettyValue (HiValueNumber n) =
  if denominator n == 1 then pretty $ numerator n
  else let (s, mb) = fromRationalRepetendUnlimited n in
    case mb of
      Just _  -> if abs n > 1
                 then helper (quotRem (numerator n) (denominator n))
                  (\x y -> pretty x <+> getSign n <+> pretty (abs y) <> pretty "/" <> pretty (denominator n))
                 else pretty (numerator n) <> pretty "/" <> pretty (denominator n)
      Nothing -> pretty $ formatScientific Fixed Nothing s
    where
      helper (x, y) f = f x y
      getSign n = pretty $ if n >= 0 then "+" else "-"

prettyValue (HiValueTime time) = pretty "parse-time(\"" <> pretty (show time) <> pretty "\")"

prettyValue (HiValueDict dict) = align (encloseSep (pretty "{") (pretty " }") (pretty ",") (Prelude.map prettyItem $ Data.Map.toList dict))

prettyItem :: (HiValue, HiValue) -> Doc AnsiStyle
prettyItem (k, v) = pretty " " <> prettyValue k <> pretty ":" <+> prettyValue v

prettyByte :: Word8 -> Doc AnsiStyle
prettyByte n = pretty " " <> cat (myMap (pretty . intToDigit) (quotRem (fromIntegral n) 16)) where
  myMap f (a, b) = [f a, f b]
