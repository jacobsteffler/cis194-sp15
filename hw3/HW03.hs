module HW03 where

data Expression =
    Var String                   -- Variable
  | Val Int                      -- Integer literal
  | Op Expression Bop Expression -- Operation
  deriving (Show, Eq)

-- Binary (2-input) operators
data Bop = 
    Plus     
  | Minus    
  | Times    
  | Divide   
  | Gt
  | Ge       
  | Lt  
  | Le
  | Eql
  deriving (Show, Eq)

data Statement =
    Assign   String     Expression
  | Incr     String
  | If       Expression Statement  Statement
  | While    Expression Statement       
  | For      Statement  Expression Statement Statement
  | Sequence Statement  Statement        
  | Skip
  deriving (Show, Eq)

type State = String -> Int

-- Exercise 1 -----------------------------------------

extend :: State -> String -> Int -> State
extend st name val = newState
    where
        newState :: State
        newState a | a == name = val | otherwise = st a

empty :: State
empty _ = 0

-- Exercise 2 -----------------------------------------

evalE :: State -> Expression -> Int
evalE st (Var str) = st str
evalE _ (Val int) = int
evalE st (Op ex1 bop ex2) = case bop of
    Plus    ->  lhs + rhs
    Minus   ->  lhs - rhs
    Times   ->  lhs * rhs
    Divide  ->  div lhs rhs
    Gt      ->  if (lhs > rhs)  then 1 else 0
    Ge      ->  if (lhs >= rhs) then 1 else 0
    Lt      ->  if (lhs < rhs)  then 1 else 0
    Le      ->  if (lhs <= rhs) then 1 else 0
    Eql     ->  if (lhs == rhs) then 1 else 0
    where
        lhs = evalE st ex1
        rhs = evalE st ex2

-- Exercise 3 -----------------------------------------

data DietStatement = DAssign String Expression
                   | DIf Expression DietStatement DietStatement
                   | DWhile Expression DietStatement
                   | DSequence DietStatement DietStatement
                   | DSkip
                     deriving (Show, Eq)

desugar :: Statement -> DietStatement
desugar s = case s of
    (Assign str ex)     -> DAssign str ex
    (Incr str)          -> DAssign str (Op (Var str) Plus (Val 1))
    (If ex s1 s2)       -> DIf ex (desugar s1) (desugar s2)
    (While ex s1)       -> DWhile ex (desugar s1)
    (For s1 ex s2 s3)   -> DSequence (desugar s1)
                            (DWhile ex
                            (DSequence (desugar s3) (desugar s2)))
    (Sequence s1 s2)    -> DSequence (desugar s1) (desugar s2)
    Skip                -> DSkip

-- Exercise 4 -----------------------------------------

evalSimple :: State -> DietStatement -> State
evalSimple st ds = case ds of
    (DAssign str ex) -> extend st str (evalE st ex)
    (DIf ex tr fa) -> if (evalE st ex) /= 0
                        then evalSimple st tr
                        else evalSimple st fa
    wh@(DWhile ex lo) -> if (evalE st ex) /= 0
                            then evalSimple st (DSequence lo wh)
                            else st
    (DSequence s1 s2) -> evalSimple (evalSimple st s1) s2
    DSkip -> st

run :: State -> Statement -> State
run st s = evalSimple st (desugar s)

-- Programs -------------------------------------------

slist :: [Statement] -> Statement
slist [] = Skip
slist l  = foldr1 Sequence l

{- Calculate the factorial of the input

   for (Out := 1; In > 0; In := In - 1) {
     Out := In * Out
   }
-}
factorial :: Statement
factorial = For (Assign "Out" (Val 1))
                (Op (Var "In") Gt (Val 0))
                (Assign "In" (Op (Var "In") Minus (Val 1)))
                (Assign "Out" (Op (Var "In") Times (Var "Out")))


{- Calculate the floor of the square root of the input

   B := 0;
   while (A >= B * B) {
     B++
   };
   B := B - 1
-}
squareRoot :: Statement
squareRoot = slist [ Assign "B" (Val 0)
                   , While (Op (Var "A") Ge (Op (Var "B") Times (Var "B")))
                       (Incr "B")
                   , Assign "B" (Op (Var "B") Minus (Val 1))
                   ]

{- Calculate the nth Fibonacci number

   F0 := 1;
   F1 := 1;
   if (In == 0) {
     Out := F0
   } else {
     if (In == 1) {
       Out := F1
     } else {
       for (C := 2; C <= In; C++) {
         T  := F0 + F1;
         F0 := F1;
         F1 := T;
         Out := T
       }
     }
   }
-}
fibonacci :: Statement
fibonacci = slist [ Assign "F0" (Val 1)
                  , Assign "F1" (Val 1)
                  , If (Op (Var "In") Eql (Val 0))
                       (Assign "Out" (Var "F0"))
                       (If (Op (Var "In") Eql (Val 1))
                           (Assign "Out" (Var "F1"))
                           (For (Assign "C" (Val 2))
                                (Op (Var "C") Le (Var "In"))
                                (Incr "C")
                                (slist
                                 [ Assign "T" (Op (Var "F0") Plus (Var "F1"))
                                 , Assign "F0" (Var "F1")
                                 , Assign "F1" (Var "T")
                                 , Assign "Out" (Var "T")
                                 ])
                           )
                       )
                  ]
