module HW3.Parser where

import           Control.Monad                  (void)
import           Control.Monad.Combinators.Expr
import           Data.ByteString                as BS
import           Data.Char                      (digitToInt)
import           Data.Ratio                     ((%))
import           Data.Scientific                (toRationalRepetend,
                                                 toRealFloat)
import           Data.Sequence                  (fromList)
import           Data.Text
import           Data.Text.Lazy.Builder.Int     (hexadecimal)
import           Data.Void
import           Data.Word                      (Word8)
import           HW3.Base
import           Numeric                        (readHex)
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer     as L

type Parser = Parsec Void String

parse :: String -> Either (ParseErrorBundle String Void) HiExpr
parse = runParser parseAll ""

sc :: Parser ()
sc = L.space
  space1
  (L.skipLineComment "//")
  (L.skipBlockComment "/*" "*/")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

parseOpGen :: String -> HiFun -> Parser HiValue
parseOpGen s f = lexeme $ HiValueFunction f <$ string s

parseOp :: Parser HiValue
parseOp =
      parseOpGen "mul" HiFunMul
  <|> parseOpGen "div" HiFunDiv
  <|> parseOpGen "add" HiFunAdd
  <|> parseOpGen "sub" HiFunSub
  <|> parseOpGen "and" HiFunAnd
  <|> parseOpGen "or" HiFunOr
  <|> parseOpGen "less-than" HiFunLessThan
  <|> parseOpGen "greater-than" HiFunGreaterThan
  <|> parseOpGen "equals" HiFunEquals
  <|> parseOpGen "not-less-than" HiFunNotLessThan
  <|> parseOpGen "not-greater-than" HiFunNotGreaterThan
  <|> parseOpGen "not-equals" HiFunNotEquals
  <|> parseOpGen "not" HiFunNot
  <|> parseOpGen "if" HiFunIf
  <|> parseOpGen "length" HiFunLength
  <|> parseOpGen "to-upper" HiFunToUpper
  <|> parseOpGen "to-lower" HiFunToLower
  <|> parseOpGen "reverse" HiFunReverse
  <|> parseOpGen "trim" HiFunTrim
  <|> parseOpGen "list" HiFunList
  <|> parseOpGen "range" HiFunRange
  <|> parseOpGen "fold" HiFunFold
  <|> parseOpGen "pack-bytes" HiFunPackBytes
  <|> parseOpGen "unpack-bytes" HiFunUnpackBytes
  <|> parseOpGen "zip" HiFunZip
  <|> parseOpGen "unzip" HiFunUnzip
  <|> parseOpGen "encode-utf8" HiFunEncodeUtf8
  <|> parseOpGen "decode-utf8" HiFunDecodeUtf8
  <|> parseOpGen "serialise" HiFunSerialise
  <|> parseOpGen "deserialise" HiFunDeserialise
  <|> parseOpGen "read" HiFunRead
  <|> parseOpGen "write" HiFunWrite
  <|> parseOpGen "mkdir" HiFunMkDir
  <|> parseOpGen "cd" HiFunChDir
  <|> parseOpGen "parse-time" HiFunParseTime
  <|> parseOpGen "rand" HiFunRand
  <|> parseOpGen "echo" HiFunEcho
  <|> parseOpGen "count" HiFunCount
  <|> parseOpGen "keys" HiFunKeys
  <|> parseOpGen "values" HiFunValues
  <|> parseOpGen "invert" HiFunInvert

parseNum :: Parser HiValue
parseNum = do
  integer <- lexeme $ L.signed sc $ lexeme L.scientific
  return $ HiValueNumber $ toRational integer

parseBool :: Parser HiValue
parseBool =
  lexeme $ HiValueBool True <$ symbol "true" <|> HiValueBool False <$ symbol "false"

parseNull :: Parser HiValue
parseNull = lexeme $ HiValueNull <$ symbol "null"

parseString :: Parser HiValue
parseString = do
  str <- lexeme $ char '"' >> manyTill L.charLiteral (char '"')
  return $ HiValueString $ Data.Text.pack str

listParens :: Parser a -> Parser a
listParens = between (symbol "[") (symbol "]")

parseList :: Parser HiExpr
parseList = do
  args <- lexeme $ listParens $ sepBy parseAutoExpr $ symbol ","
  return $ HiExprApply (HiExprValue (HiValueFunction HiFunList)) args

bytesParens :: Parser a -> Parser a
bytesParens = between (symbol "[#") (symbol "#]")

parseByte :: Parser Word8
parseByte = do
  h <- Text.Megaparsec.count 2 (hexDigitChar :: Parser Char)
  return $ fromIntegral $getHex h where
    getHex x = case x of
      [n1, n2] -> digitToInt n1 * 16 + digitToInt n2
      _        -> undefined

parseBytes :: Parser HiValue
parseBytes = do
  args <- lexeme $ bytesParens $ sepEndBy parseByte space1
  return $ HiValueBytes $ BS.pack args

parseCwd :: Parser HiValue
parseCwd = lexeme $ HiValueAction HiActionCwd <$ symbol "cwd"

parseNow :: Parser HiValue
parseNow = lexeme $ HiValueAction HiActionNow <$ symbol "now"

parseIdent :: Parser HiValue
parseIdent = do
  HiValueString . Data.Text.pack <$>
    lexeme ((:) <$> letterChar <*> many (alphaNumChar <|> char '-'))

dictParens :: Parser a -> Parser a
dictParens = between (symbol "{") (symbol "}")

parseDictArg :: Parser (HiExpr, HiExpr)
parseDictArg = do
  k <- parseAutoExpr
  symbol ":"
  v <- parseAutoExpr
  return (k, v)

parseDict :: Parser HiExpr
parseDict = do
  args <- lexeme $ dictParens $ sepBy parseDictArg $ symbol ","
  return $ HiExprDict args

parseVal' :: Parser HiValue
parseVal' = do
  parseOp <|> parseNum <|> parseBool <|> try parseString <|> parseNull <|> try parseBytes <|> parseCwd <|> parseNow

parseVal :: Parser HiExpr
parseVal = do
  (HiExprValue <$> parseVal') <|> parseDict <|> try parseList

parseArgs :: Parser [HiExpr]
parseArgs =
  lexeme $ parens $ sepBy parseAutoExpr $ symbol ","

parseDotApply :: Parser [HiExpr]
parseDotApply = do
  char '.'
  idnt <- parseIdent
  return [HiExprValue idnt]

parseTerm' :: Parser [([String], [HiExpr])]
parseTerm' = do
  g <- parseArgs <|> parseDotApply
  r <- many $ symbol "!"
  t <- optional parseTerm'
  return $ case t of
    Just t' -> (r, g):t'
    Nothing -> [(r, g)]

parseTerm :: Parser HiExpr
parseTerm = do
  v <- parseVal <|> try (parens parseAutoExpr)
  r <- many $ symbol "!"
  g <- optional parseTerm'
  let v' = Prelude.foldr (const HiExprRun) v r
  return $ case g of
    Just g' -> Prelude.foldl helper v' g'
    Nothing -> v'
    where
      helper x (runs, y) =
        Prelude.foldr (const HiExprRun) (HiExprApply x y) runs

parseAutoExpr :: Parser HiExpr
parseAutoExpr =
  let inf' assoc symb f = assoc ((\l r -> HiExprApply (HiExprValue (HiValueFunction f)) [l, r]) <$ try (symbol symb <* notFollowedBy (symbol "=")))
      infl = inf' InfixL
      infr = inf' InfixR
      infn = inf' InfixN in
  makeExprParser (try parseTerm <|> parens parseAutoExpr)  [
    [infl "/" HiFunDiv,          infl "*" HiFunMul],
    [infl "+" HiFunAdd,          infl "-" HiFunSub],
    [infn "<" HiFunLessThan,     infn ">" HiFunGreaterThan,
     infn ">=" HiFunNotLessThan, infn "<=" HiFunNotGreaterThan,
     infn "==" HiFunEquals,      infn "/=" HiFunNotEquals],
    [infr "&&" HiFunAnd],
    [infr "||" HiFunOr]
  ]

parseAll :: Parser HiExpr
parseAll = do
  sc
  parseAutoExpr <* eof
