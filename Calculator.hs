module Calculator
    ( Operator (Add, Subtract, Multiply, Divide)
    , Token (Operator, Operand)
    , evaluate
    ) where

import Control.Monad

data Operator =
    Add
    | Subtract
    | Multiply
    | Divide

data Token =
    Operand Double
    | Operator Operator

type Stack = [Double]

operate :: Operator -> Stack -> Maybe Stack
operate Add (x:y:xs) = Just (x + y : xs)
operate Add _ = Nothing

operate Subtract (x:y:xs) = Just (x - y : xs)
operate Subtract _ = Nothing

operate Multiply (x:y:xs) = Just (x * y : xs)
operate Multiply _ = Nothing

operate Divide (x:y:xs) = Just (x / y : xs)
operate Divide _ = Nothing

push :: Double -> Stack -> Stack
push x stk = x:stk

maybeHead :: Stack -> Maybe Double
maybeHead [x] = Just x
maybeHead _ = Nothing

evaluateInner :: Stack -> Token -> Maybe Stack
evaluateInner stk (Operand x) = Just (push x stk)
evaluateInner stk (Operator op) = operate op stk

evaluate :: [Token] -> Maybe Double
evaluate xs = foldM evaluateInner [] xs >>= maybeHead