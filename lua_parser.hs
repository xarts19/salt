{-
    Lua static analyzer.

    Copyright (c) 2013, Vitaly Turinsky
    All rights reserved.

    Distributed under the Boost Software License, Version 1.0. (See accompanying
    file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
-}

{-
    TODO:
    - parse all lua files together to find global names
    - all names are contained inside this files or come from my C exported functions or standard libs
    - create a file with exported and standard functions' signatures
-}

module Main where

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
             | UnOp String LuaType
             | BinOp String LuaType LuaType
    deriving (Show)


data Stmt = Block [Stmt]
          | Assign [String] [LuaType]
          | If LuaType Stmt [(LuaType, Stmt)] Stmt
          | While LuaType Stmt
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
parseExpr  = buildExpressionParser table term <?> "expression"


table = [ [Prefix (m_reservedOp "~" >> return (UnOp "~"))]
        , [Infix (m_reserved "and" >> return (BinOp "and")) AssocLeft]
        , [Infix (m_reserved "or" >> return (BinOp "or")) AssocLeft]
        ]
term = m_parens parseExpr
       <|> fmap LuaVar m_identifier
       <|> (m_reserved "true" >> return Bool)
       <|> (m_reserved "false" >> return Bool)
       <|> (m_reserved "nil" >> return Nil)
       <|> (m_naturalOrFloat >> return Number)
       <|> (m_stringLiteral >> return String)
       -- function call here
       -- operators here


parseAssign :: Parser Stmt
parseAssign = (do idents <- m_commaSep1 m_identifier
                  m_reservedOp "="
                  exprs <- m_commaSep1 parseExpr
                  option "" m_semi
                  return $ Assign idents exprs
              ) <?> "assign"


parseIf :: Parser Stmt
parseIf = (do m_reserved "if"
              expr <- parseExpr
              m_reserved "do"
              block <- many parseStmt
              elseifs <- option [] $ many $ try parseElseifs
              el <- option (Block []) $ try parseElse
              m_reserved "end"
              return $ If expr (Block block) elseifs el
          ) <?> "if"
          where parseElseifs = (do m_reserved "elseif"
                                   cond <- parseExpr
                                   m_reserved "do"
                                   block <- many parseStmt
                                   return (cond, Block block)
                               ) <?> "elseif"
                parseElse = (do m_reserved "else"
                                block <- many parseStmt
                                return $ Block block
                            ) <?> "else"


parseWhile :: Parser Stmt
parseWhile = (do m_reserved "while"
                 expr <- parseExpr
                 m_reserved "do"
                 block <- many parseStmt
                 m_reserved "end"
                 return $ While expr (Block block)
             ) <?> "while"


parseBlock :: Parser Stmt
parseBlock = (do m_reserved "do"
                 block <- many parseStmt
                 m_reserved "end"
                 return $ Block block
             ) <?> "block"


parseStmt :: Parser Stmt
parseStmt = try parseIf <|> try parseWhile <|> try parseAssign <|> try parseBlock <?> "statement"


parseLua :: Parser Stmt
parseLua = (do m_whiteSpace
               stmts <- many parseStmt
               eof
               return $ Block stmts
           ) <?> "main"


readExpr :: String -> String
readExpr input = case parse parseLua "lua" input of
    Left err -> "No match: " ++ show err
    Right val -> show val


main :: IO ()
main = do
    arguments <- getArgs
    s <- readFile $ head arguments
    (putStrLn . readExpr) s

