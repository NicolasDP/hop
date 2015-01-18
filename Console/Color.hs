-- |
-- Module      : Console.Color
-- License     : BSD-Style
-- Maintainer  : Nicolas DI PRIMA <nicolas@di-prima.fr>
-- Stability   : experimental
-- Portability : unknown
--
-- Colorized output
-- This function is a collection of helpers to print colorized string on Linux
-- like Shell
--
-- Example:
--
-- > putStrLn $ (console [Bold, Blinking, Red]) ++ "something" ++ (console [Reset])
--

{-# LANGUAGE CPP #-}

module Console.Color
    ( console
    , ShellCode(..)
    ) where

import Data.List

data ShellCode =
      Reset      -- 0
    | Bold       -- 1
    | Shadow     -- 2
    | Normal     -- 3
    | Underline  -- 4
    | Blinking   -- 5
    | Background -- 7
    | Foreground -- 8
    | Black      -- 30
    | Red        -- 31
    | Green      -- 32
    | Yellow     -- 33
    | Blue       -- 34
    | Purple     -- 35
    | Cyan       -- 36
    | White      -- 37
  deriving (Show, Eq, Ord)

toCode :: ShellCode -> Int
toCode c =
    case c of
        Reset      -> 0
        Bold       -> 1
        Shadow     -> 2
        Normal     -> 3
        Underline  -> 4
        Blinking   -> 5
        Background -> 7
        Foreground -> 8
        Black      -> 30
        Red        -> 31
        Green      -> 32
        Yellow     -> 33
        Blue       -> 34
        Purple     -> 35
        Cyan       -> 36
        White      -> 37

toStrCode :: ShellCode -> String
toStrCode = show . toCode

#ifndef WINDOWS
console :: [ShellCode] -> String
console [] = []
console l  = "\27[" ++ (intercalate ";" $ map toStrCode l) ++ "m"
#else
console :: [ShellCode] -> String
console _ = []
{-# DEPRECATED console "Just to let you know that on windows you won't have colorized output" #-}
#endif
