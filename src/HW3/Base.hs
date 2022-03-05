{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module HW3.Base where

import           Codec.Serialise (Serialise)
import qualified Control.Monad

import           Data.ByteString (ByteString)
import           Data.Map        (Map)
import           Data.Sequence   (Seq)
import           Data.Text       (Text)
import           Data.Time       (UTCTime)
import           GHC.Generics    (Generic)

data HiFun
  = HiFunDiv
  | HiFunMul
  | HiFunAdd
  | HiFunSub  -- logical
  | HiFunNot
  | HiFunAnd
  | HiFunOr
  | HiFunLessThan
  | HiFunGreaterThan
  | HiFunEquals
  | HiFunNotLessThan
  | HiFunNotGreaterThan
  | HiFunNotEquals
  | HiFunIf
  | HiFunLength  -- strings
  | HiFunToUpper
  | HiFunToLower
  | HiFunReverse
  | HiFunTrim
  | HiFunList  -- list
  | HiFunRange
  | HiFunFold
  | HiFunPackBytes -- bytes
  | HiFunUnpackBytes
  | HiFunEncodeUtf8
  | HiFunDecodeUtf8
  | HiFunZip
  | HiFunUnzip
  | HiFunSerialise
  | HiFunDeserialise
  | HiFunRead  -- files
  | HiFunWrite
  | HiFunMkDir
  | HiFunChDir
  | HiFunParseTime -- time
  | HiFunRand -- random
  | HiFunEcho -- echo
  | HiFunCount -- dict
  | HiFunKeys
  | HiFunValues
  | HiFunInvert
   deriving (Eq, Ord, Show, Generic, Serialise)

data HiAction =
    HiActionRead  FilePath
  | HiActionWrite FilePath ByteString
  | HiActionMkDir FilePath
  | HiActionChDir FilePath
  | HiActionCwd
  | HiActionNow
  | HiActionRand Int Int
  | HiActionEcho Text
  deriving (Eq, Ord, Show, Generic, Serialise)

data HiValue
  = HiValueFunction HiFun
  | HiValueBool Bool
  | HiValueNumber Rational
  | HiValueNull
  | HiValueString Text
  | HiValueList (Seq HiValue)
  | HiValueBytes ByteString
  | HiValueAction HiAction
  | HiValueTime UTCTime
  | HiValueDict (Map HiValue HiValue) deriving (Eq, Ord, Show, Generic, Serialise)

data HiExpr
  = HiExprValue HiValue
  | HiExprApply HiExpr [HiExpr]
  | HiExprRun HiExpr
  | HiExprDict [(HiExpr, HiExpr)] deriving (Eq, Show)

data HiError
  = HiErrorInvalidArgument
  | HiErrorInvalidFunction
  | HiErrorArityMismatch
  | HiErrorDivideByZero
  | HiTestError deriving (Eq, Show)

class Monad m => HiMonad m where
  runAction :: HiAction -> m HiValue
