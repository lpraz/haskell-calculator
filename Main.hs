import Calculator

main :: IO ()
main = print $ evaluate [Operand 3, Operand 4, Operator Add]