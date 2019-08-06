module Tokenizer
    ( tokenizeRpn
    ) where

import Calculator

import Text.Read (readMaybe)

tokenizeWord :: String -> Maybe Token
tokenizeWord word
    | word == "+" = Just $ Operator Add
    | word == "-" = Just $ Operator Subtract
    | word == "*" = Just $ Operator Multiply
    | word == "/" = Just $ Operator Divide
    | otherwise = (readMaybe word :: Maybe Double) >>= (\x -> Just $ Operand x)

tokenizeRpn :: String -> Maybe [Token]
tokenizeRpn str = sequence $ map tokenizeWord $ words str