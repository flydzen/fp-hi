{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns    #-}
module HW3.Evaluator where
import           Codec.Compression.Zlib     (CompressParams (compressLevel),
                                             bestCompression, compressWith,
                                             decompress, defaultCompressParams)
import           Codec.Serialise            (deserialise, serialise)
import           Control.Monad              (when)
import           Control.Monad.Except
import           Data.Bits                  (Bits (xor))
import           Data.ByteString            as BS (ByteString, drop, foldr,
                                                   head, length, pack, reverse,
                                                   singleton, take, unpack)
import           Data.ByteString.Lazy       (ByteString, toStrict)
import           Data.ByteString.Lazy.Char8 (fromStrict)
import           Data.Either                (fromLeft, fromRight, isLeft)
import           Data.Foldable              (toList)
import           Data.Map                   (Map, (!), (!?))
import qualified Data.Map
import qualified Data.Maybe
import           Data.Ratio                 (denominator, numerator, (%))
import           Data.Scientific            (scientific)
import           Data.Semigroup             (Semigroup (stimes))
import           Data.Sequence              (Seq, empty, fromList, length,
                                             replicate, reverse, (<|), (><))
import qualified Data.Sequence              as Seq
import           Data.Text                  (Text, drop, empty, foldr, last,
                                             length, pack, reverse, singleton,
                                             strip, take, takeEnd, toLower,
                                             toUpper, unpack)
import           Data.Text.Encoding         (decodeUtf8, decodeUtf8',
                                             encodeUtf8)
import           Data.Time                  (NominalDiffTime, UTCTime,
                                             addUTCTime, diffUTCTime,
                                             nominalDiffTimeToSeconds,
                                             secondsToNominalDiffTime)
import           Data.Word                  (Word8)
import           HW3.Base
import           Text.Read                  (readMaybe)

type Res' m = ExceptT HiError m
type ResH' m = Res' m HiValue

-- tryToRunAction (HiValueAction action) = lift (runAction action)

eval :: HiMonad m => HiExpr -> m (Either HiError HiValue)
eval input = runExceptT (eval' input)

eval' :: HiMonad m => HiExpr -> ResH' m
eval' (HiExprValue v) = return v
eval' (HiExprApply (HiExprValue v) args) = case v of    -- если функция вычислена
    (HiValueFunction f) -> case f of
        HiFunAdd            -> binop' args add'
        HiFunSub            -> binop' args sub'
        HiFunMul            -> binop' args mul'
        HiFunDiv            -> binop' args div'
        HiFunAnd            -> and'   args
        HiFunOr             -> or'    args
        HiFunLessThan       -> binop' args lt'
        HiFunGreaterThan    -> binop' args gt'
        HiFunNotLessThan    -> binop' args nlt'
        HiFunNotGreaterThan -> binop' args ngt'
        HiFunEquals         -> binop' args eq'
        HiFunNotEquals      -> binop' args neq'
        HiFunNot            -> unop'  args not'
        HiFunIf             -> if'    args
        HiFunLength         -> unop'  args len'
        HiFunToUpper        -> unop'  args toUp'
        HiFunToLower        -> unop'  args toLow'
        HiFunReverse        -> unop'  args reverse'
        HiFunTrim           -> unop'  args trim'
        HiFunList           -> list'  args
        HiFunRange          -> binop' args range'
        HiFunFold           -> binop' args fold'
        HiFunPackBytes      -> unop'  args packBytes'
        HiFunUnpackBytes    -> unop'  args unpackBytes'
        HiFunEncodeUtf8     -> unop'  args encodeUTF8'
        HiFunDecodeUtf8     -> unop'  args decodeUTF8'
        HiFunZip            -> unop'  args zip'
        HiFunUnzip          -> unop'  args unzip'
        HiFunSerialise      -> unop'  args serialise'
        HiFunDeserialise    -> unop'  args deserialise'
        HiFunRead           -> unop'  args read'
        HiFunWrite          -> binop' args write'
        HiFunMkDir          -> unop'  args mkdir'
        HiFunChDir          -> unop'  args cd'
        HiFunParseTime      -> unop'  args parseTime'
        HiFunRand           -> binop' args rand'
        HiFunEcho           -> unop'  args echo'
        HiFunCount          -> unop'  args counts'
        HiFunKeys           -> unop'  args keys'
        HiFunValues         -> unop'  args values'
        HiFunInvert         -> unop'  args invert'
    (HiValueString s) -> indexedBase s strIndex' strSlice' args
    (HiValueList l)   -> indexedBase l listIndex' listSlice' args
    (HiValueBytes b)  -> indexedBase b bytesIndex' bytesSlice' args
    (HiValueDict d)   -> unop' args $ dictIndex' (HiValueDict d)
    _                 -> throwError HiErrorInvalidFunction
eval' (HiExprApply ap args) = do                        -- если функция не вычислена
    ap' <- eval' ap
    eval' (HiExprApply (HiExprValue ap') args)
eval' (HiExprRun r)  = unop' [r] run'                   -- если и не обычная функция вовсе
eval' (HiExprDict d) = dict' d

exToHiLists :: HiMonad m => [HiExpr] -> Res' m [HiValue]
exToHiLists [] = return []
exToHiLists (y:ys) = do
    y' <- eval' y
    z  <- exToHiLists ys
    return $ y' : z

baseOp :: HiMonad m => [HiExpr] -> ([HiValue] -> ResH' m) -> ResH' m
baseOp ar op = do
    ar' <- exToHiLists ar
    op ar'

unop' :: HiMonad m => [HiExpr] -> (HiValue -> ResH' m) -> ResH' m
unop' ar op = baseOp ar $ \case
    [x] -> op x
    _   -> throwError HiErrorArityMismatch

binop' :: HiMonad m => [HiExpr] -> (HiValue -> HiValue -> ResH' m) -> ResH' m
binop' ar op = baseOp ar $ \case
    [x, y] -> op x y
    _      -> throwError HiErrorArityMismatch

indexedBase :: HiMonad m => a -> (a -> HiValue -> ResH' m) -> (a -> HiValue -> HiValue -> ResH' m) -> [HiExpr] -> ResH' m
indexedBase t indx slse args = baseOp args $ \case
    [x]    -> indx t x
    [x, y] -> slse t x y
    _      -> throwError HiErrorArityMismatch

list' :: HiMonad m => [HiExpr] -> ResH' m
list' x = do
    z <- exToHiLists x
    return $ HiValueList $ Seq.fromList z

add' :: HiMonad m => HiValue -> HiValue -> ResH' m
add' (HiValueNumber x) (HiValueNumber y) = return $ HiValueNumber (x + y)
add' (HiValueString x) (HiValueString y) = return $ HiValueString (x <> y)
add' (HiValueList x) (HiValueList y)     = return $ HiValueList (x >< y)
add' (HiValueBytes x) (HiValueBytes y)   = return $ HiValueBytes (x <> y)
add' (HiValueTime x) (HiValueNumber y)   = return $ HiValueTime (addUTCTime (secondsToNominalDiffTime (fromRational y)) x)
add' (HiValueNumber y) (HiValueTime x)   = add' (HiValueTime x) (HiValueNumber y)
add' (HiValueDict x) (HiValueDict y)     = return $ HiValueDict $ Data.Map.union x y
add' _ _                                 = throwError HiErrorInvalidArgument

sub' :: HiMonad m => HiValue -> HiValue -> ResH' m
sub' (HiValueNumber x) (HiValueNumber y) = return $ HiValueNumber (x - y)
sub' (HiValueTime x) (HiValueNumber y)   = add' (HiValueTime x) (HiValueNumber (-y))
sub' (HiValueTime x) (HiValueTime y)     = return $ HiValueNumber $ toRational $ nominalDiffTimeToSeconds $ diffUTCTime x y
sub' (HiValueDict x) (HiValueDict y)     = return $ HiValueDict $ Data.Map.difference x y
sub' _ _                                 = throwError HiErrorInvalidArgument

mulStruct' :: HiMonad m => (Int -> HiValue) -> a -> Rational -> ResH' m
mulStruct' op val n = if checkIsInt n && n >= 1
                      then return $ op (ratToInt n)
                      else throwError HiErrorInvalidArgument
mul' :: HiMonad m => HiValue -> HiValue -> ResH' m
mul' (HiValueNumber x) (HiValueNumber y) = return $ HiValueNumber (x * y)
mul' (HiValueNumber x) (HiValueString y) = mulStruct' (\n -> HiValueString (stimes n y)) y x
mul' (HiValueString y) (HiValueNumber x) = mul' (HiValueNumber x) (HiValueString y)
mul' (HiValueNumber x) (HiValueList y)   =
    mulStruct' (\n -> HiValueList $ Prelude.foldl (><) Seq.empty (Seq.replicate n y)) y x
mul' (HiValueList y) (HiValueNumber x)   = mul' (HiValueNumber x) (HiValueList y)
mul' (HiValueNumber x) (HiValueBytes y)  = mulStruct' (\n -> HiValueBytes (stimes n y)) y x
mul' (HiValueBytes y) (HiValueNumber x)  = mul' (HiValueNumber x) (HiValueBytes y)
mul' _ _                                 = throwError HiErrorInvalidArgument

checkIsInt :: Rational -> Bool
checkIsInt r = denominator r == 1

ratToInt :: Rational -> Int
ratToInt r = fromIntegral $ numerator r

div' :: HiMonad m => HiValue -> HiValue -> ResH' m
div' (HiValueNumber x) (HiValueNumber y) =
    if y == 0
    then throwError HiErrorDivideByZero
    else return $ HiValueNumber (x / y)
div' (HiValueString x) (HiValueString y) = return $ HiValueString (x <> Data.Text.pack "/" <> y)
div' _ _ = throwError HiErrorInvalidArgument

and' :: HiMonad m => [HiExpr] -> ResH' m
and' [x, y] = do
    x' <- eval' x
    case x' of
        (HiValueBool False) -> return x'
        HiValueNull         -> return x'
        _                   -> eval' y
and' _     = throwError HiErrorInvalidArgument

or' :: HiMonad m => [HiExpr] -> ResH' m
or' [x, y] = do
    x' <- eval' x
    case x' of
        (HiValueBool False) -> eval' y
        HiValueNull         -> eval' y
        _                   -> return x'
or' _     = throwError HiErrorInvalidArgument

lt' :: HiMonad m => HiValue -> HiValue -> ResH' m
lt' x y = return $ HiValueBool (x < y)

gt' :: HiMonad m => HiValue -> HiValue -> ResH' m
gt' x y = return $ HiValueBool (x > y)

nlt' :: HiMonad m => HiValue -> HiValue -> ResH' m
nlt' x y = return $ HiValueBool (x >= y)

ngt' :: HiMonad m => HiValue -> HiValue -> ResH' m
ngt' x y = return $ HiValueBool (x <= y)

neq' :: HiMonad m => HiValue -> HiValue -> ResH' m
neq' x y = return $ HiValueBool (x /= y)

eq' :: HiMonad m => HiValue -> HiValue -> ResH' m
eq' x y = return $ HiValueBool (x == y)

not' :: HiMonad m => HiValue -> ResH' m
not' (HiValueBool x) = return $ HiValueBool (not x)
not' _               = throwError HiErrorInvalidArgument

if' :: HiMonad m => [HiExpr] -> ResH' m
if' [a, b, c] = do
    a' <- eval' a
    case a' of
        (HiValueBool True) -> eval' b
        _                  -> eval' c
if' _ = throwError HiErrorArityMismatch

len' :: HiMonad m => HiValue -> ResH' m
len' (HiValueString x) = return $ HiValueNumber (toRational $ Data.Text.length x)
len' (HiValueList x)   = return $ HiValueNumber (toRational $ Seq.length x)
len' (HiValueBytes x)   = return $ HiValueNumber (toRational $ BS.length x)
len' _ = throwError HiErrorInvalidArgument

toUp' :: HiMonad m => HiValue -> ResH' m
toUp' (HiValueString x) = return $ HiValueString (toUpper x)
toUp' _                 = throwError HiErrorInvalidArgument

toLow' :: HiMonad m => HiValue -> ResH' m
toLow' (HiValueString x) = return $ HiValueString (toLower x)
toLow' _                 = throwError HiErrorInvalidArgument

reverse' :: HiMonad m => HiValue -> ResH' m
reverse' (HiValueString x) = return $ HiValueString (Data.Text.reverse x)
reverse' (HiValueList x)   = return $ HiValueList (Seq.reverse x)
reverse' (HiValueBytes x)  = return $ HiValueBytes (BS.reverse x)
reverse' _                 = throwError HiErrorInvalidArgument

trim' :: HiMonad m => HiValue -> ResH' m
trim' (HiValueString x) = return $ HiValueString (strip x)
trim' _                 = throwError HiErrorInvalidArgument

sliceBase :: HiMonad m => (a -> HiValue) -> (Int -> a -> a) -> (Int -> a -> a) -> (a -> Int) -> a -> Rational -> Rational -> ResH' m
sliceBase pack'' take'' drop'' len'' s n1 n2 =
    if checkIsInt n1 && checkIsInt n2
    then return $ pack'' ltxt
    else throwError HiErrorInvalidArgument where
        n1int = ratToInt n1
        n2int = ratToInt n2
        ln    = len'' s
        s'    = if   n2int < 0
                then take'' (ln + n2int) s
                else take'' n2int s
        ltxt  = if   n1int < 0
                then drop'' (ln + n1int) s'
                else drop'' n1int s'

strIndex' :: HiMonad m => Text -> HiValue -> ResH' m
strIndex' s (HiValueNumber n) = do
    st <- strSlice' s (HiValueNumber n) (HiValueNumber (n + 1))
    return $ case st of
        (HiValueString x) -> if Data.Text.length x == 1 then st else HiValueNull
        _                 -> HiValueNull
strIndex' _ _ = throwError HiErrorInvalidArgument

strSlice' :: HiMonad m => Text -> HiValue -> HiValue -> ResH' m
strSlice' s (HiValueNumber n1) (HiValueNumber n2) =
    sliceBase HiValueString Data.Text.take Data.Text.drop Data.Text.length s n1 n2
strSlice' s (HiValueNumber n1) HiValueNull = strSlice' s (HiValueNumber n1) $ HiValueNumber (toRational $ Data.Text.length s)
strSlice' s HiValueNull (HiValueNumber n2) = strSlice' s (HiValueNumber 0) (HiValueNumber n2)
strSlice' s HiValueNull HiValueNull = return $ HiValueString s
strSlice' _ _ _ = throwError HiErrorInvalidArgument

range' :: HiMonad m => HiValue -> HiValue -> ResH' m
range' (HiValueNumber n1) (HiValueNumber n2) = return $ HiValueList $ fromList $ Prelude.map HiValueNumber [n1..n2]
range' _ _ = throwError HiErrorInvalidArgument

pattern SEmpty :: Seq a
pattern SEmpty   <- (Seq.viewl -> Seq.EmptyL)
pattern (:<) :: a -> Seq a -> Seq a
pattern x :< xs <- (Seq.viewl -> x Seq.:< xs)
pattern (:>) :: Seq a -> a -> Seq a
pattern xs :> x <- (Seq.viewr -> xs Seq.:> x)

fold' :: HiMonad m => HiValue -> HiValue -> ResH' m
fold' (HiValueFunction f) (HiValueList l) = let
    helper' a b = HiExprApply (HiExprValue (HiValueFunction f)) [a, HiExprValue b] in
        case l of
            SEmpty    -> return HiValueNull
            (x :< xs) -> eval' $ Prelude.foldl helper' (HiExprValue x) xs
            _         -> undefined
fold' _ (HiValueList _) = throwError HiErrorInvalidFunction
fold' _ _ = throwError HiErrorInvalidArgument

listSlice' :: HiMonad m => Seq HiValue -> HiValue -> HiValue -> ResH' m
listSlice' s (HiValueNumber n1) (HiValueNumber n2) =
    sliceBase HiValueList Seq.take Seq.drop Seq.length s n1 n2
listSlice' s (HiValueNumber n1) HiValueNull =
    listSlice' s (HiValueNumber n1) $ HiValueNumber (toRational $ Seq.length s)
listSlice' s HiValueNull (HiValueNumber n2) =
    listSlice' s (HiValueNumber 0) (HiValueNumber n2)
listSlice' s HiValueNull HiValueNull        = return $ HiValueList s
listSlice' _ _ _ = throwError HiErrorInvalidArgument

listIndex' :: HiMonad m => Seq HiValue -> HiValue -> ResH' m
listIndex' s (HiValueNumber n) = do
    lst <- listSlice' s (HiValueNumber n) (HiValueNumber (n + 1))
    return $ case lst of
        (HiValueList (h :< SEmpty)) -> h
        _                           -> HiValueNull
listIndex' _ _ = throwError HiErrorInvalidArgument

packBytes' :: HiMonad m => HiValue -> ResH' m
packBytes' (HiValueList x) = let
    check' (HiValueNumber n) = checkIsInt n && 0 <= n && n <= 255
    check' _                 = False in
        if Prelude.all check' x
        then return $ HiValueBytes $ BS.pack $ toList $
            fmap (\(HiValueNumber n) -> fromIntegral $ ratToInt n) x
        else throwError HiErrorInvalidArgument
packBytes' _ = throwError HiErrorInvalidArgument

unpackBytes' :: HiMonad m => HiValue -> ResH' m
unpackBytes' (HiValueBytes b) = return $ HiValueList $ Seq.fromList $
    Prelude.map (HiValueNumber . toRational . fromIntegral) (BS.unpack b)
unpackBytes' _ = throwError HiErrorInvalidArgument

encodeUTF8' :: HiMonad m => HiValue -> ResH' m
encodeUTF8' (HiValueString s) = return $ HiValueBytes $ encodeUtf8 s
encodeUTF8' _                 = throwError HiErrorInvalidArgument

decodeUTF8' :: HiMonad m => HiValue -> ResH' m
decodeUTF8' (HiValueBytes b) = return $ case decodeUtf8' b of
    Right s -> HiValueString s
    Left _  -> HiValueNull
decodeUTF8' _ = throwError HiErrorInvalidArgument

zip' :: HiMonad m => HiValue -> ResH' m
zip' (HiValueBytes b) = return $ HiValueBytes $ toStrict $
    compressWith defaultCompressParams { compressLevel = bestCompression } $ fromStrict b
zip' _ = throwError HiErrorInvalidArgument

unzip' :: HiMonad m => HiValue -> ResH' m
unzip' (HiValueBytes b) = return $ HiValueBytes $ toStrict $ decompress $ fromStrict b
unzip' _ = throwError HiErrorInvalidArgument

bytesSlice' :: HiMonad m => BS.ByteString  -> HiValue -> HiValue -> ResH' m
bytesSlice' s (HiValueNumber n1) (HiValueNumber n2) =
    sliceBase HiValueBytes BS.take BS.drop BS.length s n1 n2
bytesSlice' s (HiValueNumber n1) HiValueNull =
    bytesSlice' s (HiValueNumber n1) $ HiValueNumber (toRational $ BS.length s)
bytesSlice' s HiValueNull (HiValueNumber n2) =
    bytesSlice' s (HiValueNumber 0) (HiValueNumber n2)
bytesSlice' s HiValueNull HiValueNull        =
    return $ HiValueBytes s
bytesSlice' _ _ _ = throwError HiErrorInvalidArgument

bytesIndex' :: HiMonad m => BS.ByteString -> HiValue -> ResH' m
bytesIndex' s (HiValueNumber n) = do
    bts <- bytesSlice' s (HiValueNumber n) (HiValueNumber (n + 1))
    case bts of
        (HiValueBytes bs) ->
            if BS.length bs == 1
            then return $ HiValueNumber $ toRational $ fromIntegral (BS.head bs)
            else return HiValueNull
        _ -> undefined
bytesIndex' _ _ = throwError HiErrorInvalidArgument

serialise' :: HiMonad m => HiValue -> ResH' m
serialise' o = return $ HiValueBytes $ toStrict $ serialise o

deserialise' :: HiMonad m => HiValue -> ResH' m
deserialise' (HiValueBytes b) = return $ deserialise $ fromStrict b
deserialise' _                = throwError HiErrorInvalidArgument

read' :: HiMonad m => HiValue -> ResH' m
read' (HiValueString f) =
    return $ HiValueAction $ HiActionRead (Data.Text.unpack f :: FilePath)
read'  _                =
    throwError HiErrorInvalidArgument

write' :: HiMonad m => HiValue -> HiValue -> ResH' m
write' (HiValueString f) (HiValueString s) =
    return $ HiValueAction $ HiActionWrite (Data.Text.unpack f :: FilePath) $ encodeUtf8 s
write' (HiValueString f) (HiValueBytes s)  =
    return $ HiValueAction $ HiActionWrite (Data.Text.unpack f :: FilePath) s
write' _  _                                =
    throwError HiErrorInvalidArgument

mkdir' :: HiMonad m => HiValue -> ResH' m
mkdir' (HiValueString f) =
    return $ HiValueAction $ HiActionMkDir (Data.Text.unpack f :: FilePath)
mkdir'  _                = throwError HiErrorInvalidArgument

cd' :: HiMonad m => HiValue -> ResH' m
cd' (HiValueString f) =
    return $ HiValueAction $ HiActionChDir (Data.Text.unpack f :: FilePath)
cd'  _                = throwError HiErrorInvalidArgument

run' :: HiMonad m => HiValue -> ResH' m
run' (HiValueAction action) = lift $ runAction action
run' _                      = throwError HiErrorInvalidArgument

parseTime' :: HiMonad m => HiValue -> ResH' m
parseTime' (HiValueString s) = return $
    maybe HiValueNull HiValueTime (readMaybe (Data.Text.unpack s) :: Maybe UTCTime)
parseTime' _ = throwError HiErrorInvalidArgument

rand' :: HiMonad m => HiValue -> HiValue -> ResH' m
rand' (HiValueNumber l) (HiValueNumber r) =
    if checkIsInt l && checkIsInt r && testBound l && testBound r
    then return $ HiValueAction $ HiActionRand (ratToInt l) (ratToInt r)
    else throwError HiErrorInvalidArgument where
        testBound x = toRational (minBound :: Int) < x && x < toRational (maxBound :: Int)
rand' _ _ = throwError HiErrorInvalidArgument

echo' :: HiMonad m => HiValue -> ResH' m
echo' (HiValueString l) = return $ HiValueAction $ HiActionEcho l
echo' _                 = throwError HiErrorInvalidArgument

dict' :: HiMonad m => [(HiExpr, HiExpr)] -> ResH' m
dict' items = do
    items' <- pairEval items
    return $ HiValueDict $ Data.Map.fromList items' where
        pairEval ((l, r) : xs) = do
            l'    <- eval' l
            r'    <- eval' r
            tail' <- pairEval xs
            return $ (l', r') : tail'
        pairEval [] = return []

dictIndex' :: HiMonad m => HiValue -> HiValue -> ResH' m
dictIndex' (HiValueDict d) key = do
    let val = d !? key
    return $ Data.Maybe.fromMaybe HiValueNull val
dictIndex' _ _ = throwError HiErrorInvalidFunction

keys' :: HiMonad m => HiValue -> ResH' m
keys' (HiValueDict d) = return $ HiValueList $ fromList $ Data.Map.keys d
keys' _               = throwError HiErrorInvalidArgument

values' :: HiMonad m => HiValue -> ResH' m
values' (HiValueDict d) = return $ HiValueList $ fromList $ Data.Map.elems d
values' _               = throwError HiErrorInvalidArgument

counterHelper :: HiValue -> Map HiValue HiValue -> Map HiValue HiValue
counterHelper e =
    Data.Map.insertWith (\_ (HiValueNumber y) -> HiValueNumber (y + 1)) e (HiValueNumber 1)

counts' :: HiMonad m => HiValue -> ResH' m
counts' (HiValueList l)   = return $ HiValueDict $ Prelude.foldr counterHelper Data.Map.empty l
counts' (HiValueString s) = return $ HiValueDict $
    Data.Text.foldr (counterHelper . HiValueString . Data.Text.singleton) Data.Map.empty s
counts' (HiValueBytes b)  = return $ HiValueDict $
    BS.foldr (counterHelper . HiValueNumber . toRational) Data.Map.empty b
counts' _ = throwError HiErrorInvalidArgument

invert' :: HiMonad m => HiValue -> ResH' m
invert' (HiValueDict d) =
    return $ HiValueDict $ Data.Map.foldrWithKey helper Data.Map.empty d where
        helper k e = Data.Map.insertWith
            (\(HiValueList new) (HiValueList old) -> HiValueList $ old >< new) e $
            HiValueList $ Seq.singleton k
invert' _               = throwError HiErrorInvalidArgument
