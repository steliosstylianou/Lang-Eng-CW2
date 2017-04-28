{-# LANGUAGE StandaloneDeriving #-}

import Prelude hiding (Num)
import Debug.Trace

import System.IO
import Control.Monad
import Text.ParserCombinators.Parsec hiding (State)
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token

type Num = Integer
type Var = String

type Z = Integer
type T = Bool

type State = Var -> Z

type Pname = String --name
type DecV = [(Var,Aexp)] --variable declarations
type DecP = [(Pname,Stm)] --proc name

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
          | Le Aexp Aexp deriving (Show, Eq, Read)

data Stm = Skip
         | Ass Var Aexp
         | Comp Stm Stm
         | If Bexp Stm Stm
         | While Bexp Stm
         | Block DecV DecP Stm
         | Call Pname deriving (Show)

whileParser :: Parser Stm
whileParser = whiteSpace >> statement

statement :: Parser Stm
statement =   try compStm <|> statement'

statement' :: Parser Stm
statement' =  ifStm
          <|> whileStm
          <|> skipStm
          <|> assignStm
          <|> blockStm
          <|> callStm
          <|> parens statement

compColon :: Parser (Stm -> Stm -> Stm)
compColon = try (semi) >> return Comp

compStm ::  Parser Stm
compStm  = chainr1 statement' (compColon)

ifStm :: Parser Stm
ifStm =
 do reserved "if"
    cond  <- bExpression
    reserved "then"
    stm1 <- statement
    reserved "else"
    stm2 <- statement
    return $ (If cond stm1 stm2)

whileStm :: Parser Stm
whileStm =
 do reserved "while"
    cond <- bExpression
    reserved "do"
    stm <- statement
    return $ (While cond stm)

assignStm :: Parser Stm
assignStm =
 do var  <- identifier
    reservedOp ":="
    expr <- aExpression
    return $ (Ass var expr)

skipStm :: Parser Stm
skipStm = reserved "skip" >> return Skip

blockStm :: Parser Stm
blockStm =
 do reserved "begin"
    decV <- decv
    decP <- decp
    st <- statement
    reserved "end"
    return $ (Block decV decP st)

decv :: Parser DecV
decv = many $ decv'

decv' :: Parser (Var,Aexp)
decv' =
  do reserved "var"
     var  <- identifier
     reservedOp ":="
     expr <- aExpression
     semi
     return $ (var, expr)

decp :: Parser DecP
decp = many $ decp'

decp' :: Parser (Pname,Stm)
decp' =
  do reserved "proc"
     pnm <- identifier
     reserved "is"
     stm <- statement'
     semi
     return $ (pnm, stm)

callStm :: Parser Stm
callStm =
 do reserved "call"
    pnm <- identifier
    return $ (Call pnm)

aExpression :: Parser Aexp
aExpression = buildExpressionParser aOperators aTerm

bExpression :: Parser Bexp
bExpression = buildExpressionParser bOperators bTerm

aOperators = [ [Infix  (reservedOp "*"   >> return (Mult)) AssocLeft]
            ,  [Infix  (reservedOp "+"   >> return (Add)) AssocLeft ,
               Infix  (reservedOp "-"   >> return (Sub)) AssocLeft]
             ]

bOperators = [ [Prefix (reservedOp "!" >> return (Neg))          ]
            , [Infix  (reservedOp "&" >> return (And )) AssocLeft]
            ]

bTerm =  parens bExpression
      <|> (reserved "true"  >> return (TRUE ))
      <|> (reserved "false" >> return (FALSE))
      <|> rExpression

aTerm =  parens aExpression
    <|> liftM V identifier
    <|> liftM N integer

rExpression =
   do a1 <- aExpression
      rel <- relation
      a2 <- aExpression
      return $ rel a1 a2

relation =   (reservedOp "<=" >> return Le)
        <|> (reservedOp "=" >> return Eq)

parse' :: String -> Stm
parse' str =
 case parse whileParser "" str of
      Left e  -> error $ show e
      Right r -> r

parseFile :: String -> IO Stm
parseFile file =
 do program  <- readFile file
    case parse whileParser "" program of
      Left e  -> print e >> fail "parse error"
      Right r -> return r

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
                                    , "true"
                                    , "false"
                                    , "not"
                                    , "begin"
                                    , "end"
                                    , "call"
                                    , "proc"
                                    , "is"
                                    , "var"
                                    ]
          , Token.reservedOpNames = ["+", "-", "*", ":=", "<=", "!", "&", "="]
          }

lexer = Token.makeTokenParser languageDef

identifier = Token.identifier lexer -- parses an identifier
reserved   = Token.reserved   lexer -- parses a reserved name
reservedOp = Token.reservedOp lexer -- parses an operator
parens     = Token.parens     lexer -- parses surrounding parenthesis:
                                   --   parens p
                                   -- takes care of the parenthesis and
                                   -- uses p to parse what's inside them
integer    = Token.integer    lexer -- parses an integer
semi       = Token.semi       lexer -- parses a semicolon
whiteSpace = Token.whiteSpace lexer -- parses whitespace
semisep    = Token.semiSep    lexer
symbol     = Token.symbol     lexer



--EVALUATION FUNCTIONS


n_val :: Num -> Z
n_val a = a

s :: State
s _    = 0
--s "x" = 3
--s "y" = 0
--s "z" = 0


a_val :: Aexp -> (State -> Z)
a_val (N num)           s = n_val num
a_val (V var)           s = s var
a_val (Mult aexp bexp)  s = a_val(aexp) s * a_val(bexp) s
a_val (Add aexp bexp)   s = a_val(aexp) s + a_val(bexp) s
a_val (Sub aexp bexp)   s = a_val(aexp) s - a_val(bexp) s

b_val :: Bexp -> State -> T
b_val (TRUE)          s = True
b_val (FALSE)         s = False
b_val (Neg bexp)      s = not (b_val (bexp) s)
b_val (And aexp bexp) s = (b_val (aexp) s) && (b_val (bexp) s)
b_val (Eq aexp bexp)  s = (a_val (aexp) s) == (a_val (bexp) s)
b_val (Le aexp bexp)  s = (a_val (aexp) s) <=  (a_val (bexp) s)

update :: State -> Z -> Var -> State
update st i v v2
      | v == v2 = i
      | otherwise = (s v2)

scope_stm = Block [("x",N 0)] [("p",Ass "x" (Mult (V "x") (N 2))),("q",Call "p")] (Block [("x",N 5)] [("p",Ass "x" (Add (V "x") (N 1)))] (Comp (Call "q") (Ass "y" (V "x"))))

type Loc = Integer
data Config = Inter Stm State | Final State
type Store = Loc -> Z  --store location's value
type EnvV = Var -> Loc -- store variable's location
type EnvP = Pname -> Stm

next = 0

--s_static :: Stm -> State -> State
--s_mixed  :: Stm -> State -> State
--s_dynamic :: Stm -> State -> State

new :: Loc->Loc
new loc = loc + 1

update_store :: Store -> Z -> Var -> State
update_store st i v v2
      | v == v2 = i
      | otherwise = (s v2)


lookup :: EnvV -> Store -> State
lookup envv sto = sto . envv

s_static :: Stm -> State -> State
s_static stm s = undefined



s_dynamic :: Stm -> State -> State
s_dynamic stm s = s'
 where
  Final s' = d_eval (Inter stm s) 

d_eval :: EnvV -> EnvP -> Config -> Config --pg 57
d_eval envv envp (Inter (Ass x a) s) = Final (update s (a_val a s) x)
d_eval envv envp (Inter (Skip) s) = Final s
d_eval envv envp (Inter (Comp s1 s2) s) = d_eval envv envp (Inter s2 s) . d_eval envv envp (Inter s1 s)
d_eval envv envp (Inter (If bexp s1 s2) s) 
                          | b_val bexp == True  = d_eval envp (Inter s1) s
                          | b_val bexp == False = d_eval envp (Inter s2) s
                            where 
d_eval envv envp (Inter (While b ss) s)
                          | b_val b == True  = Final s2
                          | b_val b == False = d_eval envp (Skip) s
                           where 
                           Final s1 = d_eval (Inter ss s)
                           Final s2 = d_eval (Inter (While b ss) s1)
d_eval envv envp (Inter (Block decv decp stm) s) = d_eval envv (d_updateEnvPs envp envp stm) Inter stm 
d_eval envv envp (Inter (Call name) s) = d_eval (Inter (envp name) s)


d_updateEnvPs :: EnvP -> DecP -> EnvP
d_updateEnvPs env ((pname,stms):others)  = d_updateEnvP others (envP_check env stms pname)	--pg55
d_updateEnvPs env [] = env

d_updateEnvP :: Envp -> Pname -> Stm -> Envp
d_updateEnvP (pname,stm) p s 
                 | pname == p = s
                 | otherwise  = (pname,stm)

d_updateEnvPs :: DecV -> State -> State
d_updateEnvPs decv state   = d_updateEnvP others (envP_check env stms pname)	--pg55
d_updateEnvPs env [] = env

d_updateEnvP :: Envp -> Pname -> Stm -> Envp
d_updateEnvP (pname,stm) p s 
                 | pname == p = s
                 | otherwise  = (pname,stm)
























