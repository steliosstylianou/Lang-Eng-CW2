{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE MultiWayIf #-}

import Prelude hiding (Num)
import Debug.Trace

import System.IO
import Control.Monad
import Text.ParserCombinators.Parsec hiding (State, parse)
import qualified Text.ParserCombinators.Parsec as Pars
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token
import Data.List
import Data.Maybe

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

parse :: String -> Stm
parse str =
 case Pars.parse whileParser "" str of
      Left e  -> error $ show e
      Right r -> r

{-- parseFile :: String -> IO Stm
parseFile file =
 do program  <- readFile file
    case parse whileParser "" program of
      Left e  -> print e >> fail "parse error"
      Right r -> return r
      --}

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


------------------------------------------------------------------------
------------------------------------------------------------------------

--EVALUATION FUNCTIONS


n_val :: Num -> Z
n_val a = a

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
      | otherwise = (st v2)


------------------------------------------------------------------------
------------------------------------------------------------------------

type Loc = Integer
data Config = Inter Stm State | Final State
type Store = Loc -> Z  --store location's value
type EnvV = Var -> Loc -- store variable's location

next = 0

------------------------------------------------------------------------
-- STATIC
------------------------------------------------------------------------
{--
new :: Loc->Loc
new loc = loc + 1

upd_P ::
update_store :: Store -> Z -> Var -> State
update_store st i v v2
      | v == v2 = i
      | otherwise = (s v2)


lookup :: EnvV -> Store -> State
lookup envv sto = sto . envv

s_static :: Stm -> State -> State
s_static stm s = undefined

s_eval :: EnvV -> EnvP -> Config -> Config --pg 57
s_eval envv envp (Inter (Ass x a) s) = Final (update s (a_val a s) x)
s_eval envv envp (Inter (Skip) s) = Final s
s_eval envv envp (Inter (Comp s1 s2) s) = s_eval envv envp (Inter s2 s) . s_eval envv envp (Inter s1 s)
s_eval envv envp (Inter (If bexp s1 s2) s)
                          | b_val bexp == True  = s_eval envv envp (Inter s1 s)
                          | b_val bexp == False = s_eval envv envp (Inter s2 s)
s_eval envv envp (Inter (While b ss) s)
                          | b_val b == True  = Final s2
                          | b_val b == False = s_eval envv envp (Skip) s
                           where
                           Final s1 = s_eval envv envp ((Inter ss s) s)
                           Final s2 = s_eval (Inter (While b ss) s1)
s_eval envv envp (Inter (Block decv decp stm) s) = undefined --d_eval envv (d_updateEnvPs envp envp stm) Inter stm
s_eval envv envp (Inter (Call name) s) = undefined --d_eval (Inter (envp name) s)

--}

------------------------------------------------------------------------
-- DYNAMIC
------------------------------------------------------------------------
type EnvP_d = Pname -> Stm

dynamic_state :: State
dynamic_state "x" = 5
dynamic_state _ = 0

dynamic_env :: EnvP_d
dynamic_env _ = undefined

s_dynamic :: Stm -> State -> State
s_dynamic stm s = state
          where
          Final state = d_eval dynamic_env (Inter stm s )

var_state_d :: Var -> Stm -> Integer
var_state_d v stm = state v
          where state = s_dynamic stm dynamic_state

d_eval :: EnvP_d -> Config -> Config --pg 57
d_eval  envp (Inter (Ass x a) s) = Final (update s (a_val a s) x)
d_eval  envp (Inter (Skip) s) = Final s
d_eval  envp (Inter (Comp stm1 stm2) s) = Final s2
                             where
                             Final s1 = d_eval envp (Inter stm1 s)
                             Final s2 = d_eval envp (Inter stm2 s1)
d_eval  envp (Inter (If bexp s1 s2) s)
                          | b_val bexp s = d_eval envp (Inter s1 s)
                          | otherwise = d_eval envp (Inter s2 s)
d_eval envp (Inter (While b ss) s)
                          | b_val b s  = Final s2
                          | otherwise  = Final s
                           where
                           Final s1 = d_eval envp (Inter ss s)
                           Final s2 = d_eval envp (Inter (While b ss) s1)
d_eval  envp (Inter (Call name) s) = d_eval envp (Inter (envp name) s)
d_eval  envp (Inter (Block decv decp stm) s) = Final new_state
                                              where
                                              new_state = d_updateBlock s (map fst decv) state'
                                              Final state' = (d_eval (d_updateDps envp decp) (Inter stm (d_updateDvs s decv)))
--pg 52
d_updateBlock :: State -> [Var] -> State -> State
d_updateBlock s1 [] s2 = s2
d_updateBlock s1 (var:vars) s2 = d_updateBlock s1 vars (update s2 (s1 var) var)

--pg 55
d_updateDvs :: State -> DecV -> State
d_updateDvs state ((var,aexp):others) = d_updateDvs (d_updateDv state (var,aexp)) others
d_updateDvs state [] = state

d_updateDv :: State -> (Var, Aexp) -> State
d_updateDv state (var,aexp) = \p -> if
                            | p == var -> a_val aexp state
                            | otherwise  -> state p

d_updateDps :: EnvP_d -> DecP -> EnvP_d
d_updateDps env ((pname,stms):decps) = d_updateDps (d_updateDp env (pname, stms)) decps  --pg55
d_updateDps env [] = env

d_updateDp :: EnvP_d -> (Pname, Stm) -> EnvP_d
d_updateDp envp (pname,stm) = \p -> if
                            | p == pname -> stm
                            | otherwise  -> envp p

scope_test :: Stm
scope_test = Block [("x",N 0)] [("p",Ass "x" (Mult (V "x") (N 2))),("q",Call "p")] (Block [("x",N 5)] [("p",Ass "x" (Add (V "x") (N 1)))] (Comp (Call "q") (Ass "y" (V "x"))))
fac_while:: Stm
fac_while = (Comp (Ass "y" (N 1)) (While (Neg (Eq (V "x") (N 1))) (Comp (Ass "y" (Mult (V "y") (V "x"))) (Ass "x" (Sub (V "x") (N 1))))))


------------------------------------------------------------------------
-- MIXED
------------------------------------------------------------------------
type EnvP_m = Pname -> Stm
mixed_state :: State
mixed_state "x" = 5
mixed_state _ = 0

mixed_env :: EnvP
mixed_env _ =

s_mixed :: Stm -> State -> State
s_mixed stm s = state
          where
          Final state = d_eval dynamic_env (Inter stm s )

d_eval :: EnvV -> EnvP -> Config -> Config --pg 57

var_state_d :: Var -> Stm -> Integer
var_state_d v stm = state v
          where state = s_dynamic stm dynamic_state
