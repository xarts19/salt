{-
    Lua static analyzer.

    Copyright (c) 2013, Vitaly Turinsky
    All rights reserved.

    Distributed under the Boost Software License, Version 1.0. (See accompanying
    file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
-}
module Main where

import Control.Applicative((<*))
import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Expr
import Text.Parsec.Token
import Text.Parsec.Language
import System.Environment
import Control.Monad

data TableElem = KeyVal LuaType LuaType
               | IdxVal LuaType
    deriving (Show)

data LuaType = Nil
             | Bool
             | Number
             | String
             | Function { args::[LuaType], retval::[LuaType] }
             | Userdata String
             | Thread
             | Table [TableElem]
             | LuaVar String
    deriving (Show)


data Stmt = Assign [String] [LuaType]
    deriving (Show)


def = emptyDef{ commentStart = "--[["
              , commentEnd = "]]--"
              , commentLine = "--"
              , identStart = letter <|> char '_'
              , identLetter = alphaNum <|> char '_'
              , opStart = oneOf "+-*/%^#=~<>;:,."
              , opLetter = oneOf "+-*/%^#=~<>;:,."
              , reservedOpNames = ["+", "-", "*", "/", "%", "^", "#", "==", "~=", "<=", ">=", "<", ">", "=", "(", ")", "{", "}", "[", "]", ";", ":", ",", ".", "..", "..."]
              , reservedNames = ["and", "break", "do", "else", "elseif", "end", "false", "for", "function", "if", "in", "local", "nil", "not", "or", "repeat", "return", "then", "true", "until", "while"]
              , caseSensitive = True
              }


lexer :: TokenParser ()
lexer = makeTokenParser def


TokenParser{ symbol = m_symbol
           , parens = m_parens
           , identifier = m_identifier
           , reservedOp = m_reservedOp
           , reserved = m_reserved
           , semi = m_semi
           , semiSep1 = m_semiSep1
           , commaSep1 = m_commaSep1
           , naturalOrFloat = m_naturalOrFloat
           , stringLiteral = m_stringLiteral
           , whiteSpace = m_whiteSpace } = makeTokenParser def


parseExpr :: Parser LuaType
parseExpr = (m_identifier >>= (return . LuaVar))
        <|> (m_naturalOrFloat >> return Number)
        <|> (m_stringLiteral >> return String)
        <|> ((try (m_symbol "true") <|> try (m_symbol "false")) >> return Bool)
        <|> (try (m_symbol "nil") >> return Nil)


parseStmt :: Parser Stmt
parseStmt = do idents <- m_commaSep1 m_identifier
               m_symbol "="
               exprs <- m_commaSep1 parseExpr
               option "" m_semi
               return $ Assign idents exprs


parseLua :: Parser [Stmt]
parseLua = do m_whiteSpace
              stmts <- many parseStmt
              eof
              return stmts


readExpr :: String -> String
readExpr input = case parse parseLua "lua" input of
    Left err -> "No match: " ++ show err
    Right val -> show val


main :: IO ()
main = do
    args <- getArgs
    s <- readFile $ head args
    (putStrLn . readExpr) $ s

