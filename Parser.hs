module Parser
    ( parse
    ) where

import Calculator

import Control.Monad (msum)
import Text.Read (readMaybe)

parseWord :: String -> Maybe Token
parseWord word
    | word == "+" = Just $ Operator Add
    | word == "-" = Just $ Operator Subtract
    | word == "*" = Just $ Operator Multiply
    | word == "/" = Just $ Operator Divide
    | otherwise = (readMaybe word :: Maybe Double) >>= (\x -> Just $ Operand x)

parsePostfix :: String -> Maybe [Token]
parsePostfix str = sequence $ map parseWord $ words str

parse :: String -> Maybe [Token]
parse str = msum $ map (\f -> f str) [parsePostfix]