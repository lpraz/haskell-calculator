import Calculator (evaluate)
import Parser (parse)

calculate :: String -> Maybe Double
calculate str = do
    tokens <- parse str
    evaluate tokens

main :: IO ()
main = do
    expr <- getLine
    if expr == "quit"
        then return ()
        else do
            print $ calculate expr
            main