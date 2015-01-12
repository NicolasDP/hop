{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module      : Data.String.Parse
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : portable
--
-- A very simple string parser related to Parsec and Attoparsec
--
-- Simple example:
--
-- > > parse ((,) <$> take 2 <*> char ' ' <*> (string "abc" *> anyChar)) "xx abctest"
-- > ParseOK "est" ("xx", 't')
--
module Data.String.Parse
    ( Parser
    , Result(..)
    -- * run the Parser
    , parse
    , parseFeed
    -- * Parser methods
    , isEndOfBuff
    , char
    , anyChar
    , string
    , take
    , takeWhile
    , skip
    , skipWhile
    ) where

import Control.Applicative
import Control.Monad
import Prelude hiding (take, takeWhile)

-- | Simple parsing result, that represent respectively:
--
-- * failure: with the error message
-- 
-- * continuation: that need for more input data
--
-- * success: the remaining unparsed data and the parser value
data Result a =
      ParseFail String
    | ParseMore (String -> Result a)
    | ParseOK   String a

instance Show a => Show (Result a) where
    show (ParseFail err) = "ParseFailure: " ++ err
    show (ParseMore _)   = "ParseMore _"
    show (ParseOK b a)   = "ParseOK " ++ show a ++ " " ++ show b

type Failure r = String -> String -> Result r
type Success a r = String -> a -> Result r

-- | Simple String parser structure
newtype Parser a = Parser
    { runParser :: forall r . String -> Failure r -> Success a r -> Result r }

instance Monad Parser where
    fail errorMsg = Parser $ \buf err _ -> err buf ("failed: " ++ errorMsg)
    return v = Parser $ \buf _ ok -> ok buf v
    m >>= k = Parser $ \buf err ok ->
         runParser m buf err (\buf' a -> runParser (k a) buf' err ok)
instance MonadPlus Parser where
    mzero = fail "Parser.MonadPlus.mzero"
    mplus f g = Parser $ \buf err ok ->
        -- rewrite the err callback of @f to call @g
        runParser f buf (\_ _ -> runParser g buf err ok) ok
instance Functor Parser where
    fmap f p = Parser $ \buf err ok ->
        runParser p buf err (\b a -> ok b (f a))
instance Applicative Parser where
    pure      = return
    (<*>) d e = d >>= \b -> e >>= \a -> return (b a)
instance Alternative Parser where
    empty = fail "Parser.Alternative.empty"
    (<|>) = mplus
    
-- | Run a parser on an @initial String.
--
-- If the Parser need more data than available, the @feeder function
-- is automatically called and fed to the More continuation.
parseFeed :: Monad m => m String -> Parser a -> String -> m (Result a)
parseFeed feeder p initial = loop $ parse p initial
  where loop (ParseMore k) = feeder >>= (loop . k)
        loop r             = return r

-- | Run a Parser on a String and return a 'Result'
parse :: Parser a -> String -> Result a
parse p s = runParser p s (\_ msg -> ParseFail msg) (\b a -> ParseOK b a)

------------------------------------------------------------
getMore :: Parser ()
getMore = Parser $ \buf err ok -> ParseMore $ \nextChunk ->
    if null nextChunk
        then err buf "EOL: need more data"
        else ok (buf ++ nextChunk) ()

------------------------------------------------------------

isEndOfBuff :: Parser Bool
isEndOfBuff = Parser $ \buf _ ok ->
    case buf of
        [] -> ok buf True
        _  -> ok buf False

-- | Get the next byte from the parser
anyChar :: Parser Char
anyChar = Parser $ \buf err ok ->
    case buf of
        []    -> runParser (getMore >> anyChar) buf err ok
        c1:b2 -> ok b2 c1

-- | Parse a specific byte at current position
--
-- if the byte is different than the expected on,
-- this parser will raise a failure.
char :: Char -> Parser ()
char w = Parser $ \buf err ok ->
    case buf of
        []                -> runParser (getMore >> char w) buf err ok
        c1:b2 | c1 == w   -> ok b2 () 
              | otherwise -> err buf ("byte " ++ show w ++ " : failed")

-- | Parse a sequence of bytes from current position
--
-- if the following bytes don't match the expected
-- string completely, the parser will raise a failure
string :: String -> Parser ()
string allExpected = consumeEq allExpected
  where errMsg = "string " ++ show allExpected ++ " : failed"

        -- partially consume as much as possible or raise an error.
        consumeEq expected = Parser $ \actual err ok ->
            case isMatch actual expected of
                Left   e   -> err actual e
                Right aRem -> ok aRem ()

        isMatch [] [] = Right ""
        isMatch r  [] = Right r
        isMatch [] _  = Left (errMsg ++ " : too short")
        isMatch (x:xs) (y:ys)
            | x == y    = isMatch xs ys
            | otherwise = Left (errMsg ++ " : mismatch")

------------------------------------------------------------

-- | Take @n bytes from the current position in the stream
--
-- FIXME optimize
take :: Int -> Parser [Char]
take n = Parser $ \buf err ok ->
    let (b1,b2) = splitAt n buf
     in if length b1 == n
            then ok b2 b1
            else runParser (getMore >> take n) buf err ok
        
-- | Take bytes while the @predicate hold from the current position in the stream
takeWhile :: (Char -> Bool) -> Parser [Char]
takeWhile predicate = Parser $ \buf err ok ->
    case span predicate buf of
        (_, "")  -> runParser (getMore >> takeWhile predicate) buf err ok
        (b1, b2) -> ok b2 b1

-- | Skip @n bytes from the current position in the stream
skip :: forall a. (Num a, Eq a) => a -> Parser ()
skip num = Parser $ \buf err ok ->
    case dropOf num buf of
        Left n'    -> runParser (getMore >> skip n') "" err ok
        Right buf' -> ok buf' ()
  where dropOf 0 s      = Right s
        dropOf n []     = Left n
        dropOf n (_:xs) = dropOf (n-1) xs
           
-- | Skip bytes while the @predicate hold from the current position in the stream
skipWhile :: (Char -> Bool) -> Parser ()
skipWhile p = Parser $ \buf err ok ->
    case span p buf of
        (_, "") -> runParser (getMore >> skipWhile p) [] err ok
        (_, b2) -> ok b2 ()
