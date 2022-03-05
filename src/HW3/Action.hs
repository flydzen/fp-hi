module HW3.Action where
import           Control.Exception      (Exception, throwIO)
import qualified Control.Monad
import           Control.Monad.Except   ()
import           Control.Monad.IO.Class (liftIO)
import           Data.ByteString        (ByteString)
import           Data.ByteString        as BS (readFile, writeFile)
import qualified Data.ByteString        as Data
import           Data.Map               (Map)
import           Data.Sequence          (fromList)
import           Data.Set               (Set, member)
import           Data.Text              (Text, pack, unpack)
import           Data.Text.Encoding     (decodeUtf8, decodeUtf8')
import           Data.Time              (UTCTime)
import           Data.Time.Clock        (getCurrentTime)
import           GHC.Generics           (Generic)
import           HW3.Base               (HiAction (..), HiMonad (runAction),
                                         HiValue (HiValueBytes, HiValueList, HiValueNull, HiValueNumber, HiValueString, HiValueTime))
import           System.Directory       (createDirectory, doesFileExist,
                                         getCurrentDirectory, listDirectory,
                                         setCurrentDirectory)
import           System.Random          (Random (randomR), getStdRandom)
import           System.Random.Stateful (applyAtomicGen)

data HiPermission =
    AllowRead
  | AllowWrite
  | AllowTime deriving (Eq, Ord, Show, Enum, Bounded)

data PermissionException =
  PermissionRequired HiPermission deriving (Eq, Show)

instance Exception PermissionException

newtype HIO a = HIO { runHIO :: Set HiPermission -> IO a }

instance Functor HIO where
  fmap func (HIO hio) = HIO f where
    f perm = do
      func <$> hio perm

instance Applicative HIO where
  pure func = HIO (\p -> return func)
  p <*> q = Control.Monad.ap p q

instance Monad HIO where
  (>>=) (HIO hio) func = HIO f where
    f perm = do
      res <- hio perm
      runHIO (func res) perm

instance HiMonad HIO where
  runAction (HiActionRead fp) = HIO f where
    f perm = if member AllowRead perm
             then do
               isFile <- doesFileExist fp
               if isFile
               then do
                 dt <- BS.readFile fp
                 return $ case decodeUtf8' dt of
                   Left err -> HiValueBytes dt
                   Right de -> HiValueString de
               else do
                 files <- listDirectory fp
                 return $ HiValueList $ Data.Sequence.fromList $ Prelude.map (\x -> HiValueString (Data.Text.pack (x:: String))) files
             else throwIO (PermissionRequired AllowRead)
  runAction (HiActionWrite fp bs) = HIO f where
    f perm = if member AllowWrite perm
             then do
               BS.writeFile fp bs
               return HiValueNull
             else throwIO (PermissionRequired AllowWrite)
  runAction (HiActionMkDir fp) = HIO f where
    f perm = if member AllowWrite perm
             then do
               createDirectory fp
               return HiValueNull
             else throwIO (PermissionRequired AllowWrite)
  runAction (HiActionChDir fp) = HIO f where
    f perm = if member AllowRead perm
             then do
               setCurrentDirectory fp
               return HiValueNull
             else throwIO (PermissionRequired AllowRead)
  runAction HiActionCwd = HIO f where
    f perm = if member AllowRead perm
             then do
               dt <- getCurrentDirectory
               return $ HiValueString (Data.Text.pack (dt :: String))
             else throwIO (PermissionRequired AllowRead)
  runAction HiActionNow = HIO f where
    f perm = if member AllowTime perm
             then do
               HiValueTime <$> getCurrentTime
             else throwIO (PermissionRequired AllowTime)
  runAction (HiActionRand l r) = HIO f where
    f perm = do
      let gener = getStdRandom (randomR (l, r))
      HiValueNumber . toRational <$> (gener :: IO Int)
  runAction (HiActionEcho s) = HIO f where
    f perm = if member AllowWrite perm
             then do
               Prelude.putStrLn $ Data.Text.unpack s
               return HiValueNull
             else throwIO (PermissionRequired AllowWrite)
