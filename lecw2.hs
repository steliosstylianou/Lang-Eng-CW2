type Num = Integer
type Var = String
type Pname = String
type DecV = [(Var,Aexp)]
type DecP = [(Pname,Stm)]
data Aexp = N Num | V Var | Mult Aexp Aexp
            | Add Aexp Aexp | Sub Aexp Aexp
data Bexp = TRUE | FALSE | Neg Bexp | And Bexp Bexp
            | Le Aexp Aexp | Eq Aexp Aexp
data Stm = Skip | Ass Var Aexp | Comp Stm Stm
          | If Bexp Stm Stm | While Bexp Stm
          | Block DecV DecP Stm | Call Pname1
