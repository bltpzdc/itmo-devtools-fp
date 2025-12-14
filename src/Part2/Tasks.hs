module Part2.Tasks where

import Util(notImplementedYet)

data BinaryOp = Plus | Minus | Times deriving (Show, Eq)

data Term = IntConstant { intValue :: Int }          -- числовая константа
          | Variable    { varName :: String }        -- переменная
          | BinaryTerm  { op :: BinaryOp, lhv :: Term, rhv :: Term } -- бинарная операция
             deriving(Show,Eq)

-- Для бинарных операций необходима не только реализация, но и адекватные
-- ассоциативность и приоритет
(|+|) :: Term -> Term -> Term
(|+|) t1 t2 = BinaryTerm Plus t1 t2 
(|-|) :: Term -> Term -> Term
(|-|) t1 t2 = BinaryTerm Minus t1 t2 
(|*|) :: Term -> Term -> Term
(|*|) t1 t2 = BinaryTerm Times t1 t2

infixl 6 |+|
infixl 6 |-|
infixl 7 |*|

-- Заменить переменную `varName` на `replacement`
-- во всём выражении `expression`
replaceVar :: String -> Term -> Term -> Term
replaceVar _ _ t@(IntConstant _) = t

replaceVar varName replacement (Variable v)
   | v == varName = replacement
   | otherwise = Variable v

replaceVar varName replacement (BinaryTerm op lhv rhv) =
   BinaryTerm op
      (replaceVar varName replacement lhv)
      (replaceVar varName replacement rhv)

-- Посчитать значение выражения `Term`
-- если оно состоит только из констант
evaluate :: Term -> Term
evaluate t@(IntConstant _) = t
evaluate t@(Variable _)    = t

evaluate (BinaryTerm op lhv rhv) =
   let lhv' = evaluate lhv
       rhv' = evaluate rhv
   in
      case (lhv', rhv') of
         (IntConstant l, IntConstant r) ->
            IntConstant $
               case op of
                  Plus -> l + r
                  Minus -> l - r
                  Times -> l * r

         _ ->
            BinaryTerm op lhv' rhv'
