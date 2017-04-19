{-# LANGUAGE StandaloneDeriving #-}
module Main (main) where

import Prelude hiding (Num)
import qualified Prelude (Num)
import System.IO
import Control.Monad
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token

--Syntactic definitions
type Num = Integer
type Var = String

--Semantic definitions
type Z = Integer
type T = Bool

--define state as function that maps variables to Integers (Z)
type State = Var -> Z

--Procedures Extension
type Pname = String
type DecV = [(Var,Aexp)]
type DecP = [(Pname,Stm)]

data Aexp = N Num
          | V Var
          | Mult Aexp Aexp
          | Add Aexp Aexp
          | Sub Aexp Aexp deriving (Show, Eq, Read)

data Bexp = TRUE
          | FALSE
          | Neg Bexp
          | And Bexp Bexp
          | Eq Aexp Aexp
          | Le Aexp Aexp
          | Imp Bexp Bexp deriving (Show, Eq, Read)

data Stm = Skip
         | Ass Var Aexp
         | Comp Stm Stm
         | If Bexp Stm Stm
         | While Bexp Stm
         | Block DecV DecP Stm
         | Call Pname deriving (Show)

statement :: Parser Stm
statement = parens statement <|> multStatements


statement' :: Parser Stm
statement' =  ifS
          <|> whileS
          <|> skipS
          <|> assignS
          <|> pr
          <|> declst

multStatements :: Parser Stmt
multStatements = do
        list <- sepEndBy1 statement' semi
        return $ if length list == 1 then head list else Chain list















parse' :: String -> Stmt
parse' string =
                 case parse parseProc "" string of
                     Left e -> error $ show e
                     Right r -> r
             where
                 parseProc = whiteSpace >> statement
--LEXER
languageDef =
    emptyDef { Token.commentStart    = "/*"
             , Token.commentEnd      = "*/"
             , Token.commentLine     = "//"
             , Token.identStart      = letter
             , Token.identLetter     = alphaNum
             , Token.reservedNames   = [ "if"
                                       , "then"
                                       , "else"
                                       , "while"
                                       , "do"
                                       , "skip"
                                       , "begin"
                                       , "end"
                                       , "call"
                                       , "proc"
                                       , "is"
                                       , "var"
                                       ]
             , Token.reservedOpNames = [ "+"
                                       , "-"
                                       , "*"
                                       , "/"
                                       , ":="
                                       , ">="
                                       , "="
                                       , "++"
                                       ]
             }

lexer = Token.makeTokenParser languageDef

identifier    = Token.identifier    lexer
reserved      = Token.reserved      lexer
reservedOp    = Token.reservedOp    lexer
parens        = Token.parens        lexer
integer       = Token.integer       lexer
semi          = Token.semi          lexer
comma         = Token.comma         lexer
whiteSpace    = Token.whiteSpace    lexer
stringLiteral = Token.stringLiteral lexer
