import System.Environment
import Parser.Parsec
import Antlr4.Parser

main = do
    [fn] <- getArgs
    text <- readFile fn
    putStr text
