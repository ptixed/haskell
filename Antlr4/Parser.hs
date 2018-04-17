module Antlr4.Parser where

import Parser.Parsec

lexer :: ParserT String () [Char]
lexer = concat <$>
    (mComment *> spaces *> declaration *> spaces *> many line <* eof) 

mComment = string "/*" *> mCommentBody
mCommentBody = (++) <$> many (noneOf "*") <*> mCommentEnd
mCommentEnd = (string "*/" *> pure "") <|> ((++) <$> string "*" <*> mCommentBody)

sComment = string "//" *> many (noneOf "\n") <* ((char '\n' *> pure ()) <|> eof)

declaration = string "lexer grammar" *> many (noneOf ";") <* char ';' 

line = (production <|> (sComment *> pure "")) <* ((char '\n' *> pure ()) <|> eof) 

production = (\x y -> x ++ " " ++ y ++ "\n") <$> (many alphaNum <* char ':' <* spaces <* char '\'') <*> (many (noneOf "'") <* string "';" <* spaces)

