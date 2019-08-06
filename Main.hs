import Calculator (evaluate)
import Tokenizer (tokenizeRpn)

calculate :: String -> Maybe Double
calculate str = do
    tokens <- tokenizeRpn str
    evaluate tokens

main :: IO ()
main = do
    expr <- getLine
    if expr == "quit"
        then return ()
        else do
            print $ calculate expr
            main