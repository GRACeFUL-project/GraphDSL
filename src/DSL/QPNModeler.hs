module DSL.QPNModeler where

import Interfaces.MZAST
import Interfaces.MZPrinter
import DSL.GraphDSL
import DSL.SolverExports
import Interfaces.MZinHaskell
import Data.List

signToInt :: Sign -> Int
signToInt M = 1
signToInt Z = 2
signToInt P = 3
signToInt Q = 4

-- Transition function for automaton A_+
transitionPlus :: Item
transitionPlus
  = Declare (Par, Array [Int,Int] (Par, Int)) "trans_plus"
            (Just (ArrayLit2D [[IConst 2, IConst 0, IConst 3, IConst 4]
                              ,[IConst 5, IConst 0, IConst 4, IConst 4]
                              ,[IConst 4, IConst 0, IConst 6, IConst 4]
                              ,[IConst 4, IConst 0, IConst 4, IConst 7]
                              ,[IConst 5, IConst 0, IConst 4, IConst 4]
                              ,[IConst 4, IConst 0, IConst 6, IConst 4]
                              ,[IConst 4, IConst 0, IConst 4, IConst 7]]))

-- Transition function for automaton A_x_+
transitionComb :: Item
transitionComb
--  = Declare (Par, Array [Range (IConst 1) (IConst 11), Range (IConst 1) (IConst 4)] (Par, Int)) "trans_comb"
  = Declare (Par, Array [Int, Int] (Par, Int)) "trans_comb"
            (Just (ArrayLit2D [[IConst 2 , IConst 0, IConst 4 , IConst 3 ]
                              ,[IConst 8 , IConst 0, IConst 5 , IConst 6 ]
                              ,[IConst 6 , IConst 0, IConst 6 , IConst 6 ]
                              ,[IConst 5 , IConst 0, IConst 7 , IConst 6 ]
                              ,[IConst 10, IConst 0, IConst 6 , IConst 6 ]
                              ,[IConst 6 , IConst 0, IConst 6 , IConst 10]
                              ,[IConst 5 , IConst 0, IConst 11, IConst 6 ]
                              ,[IConst 8 , IConst 0, IConst 9 , IConst 6 ]
                              ,[IConst 10, IConst 0, IConst 6 , IConst 6 ]
                              ,[IConst 6 , IConst 0, IConst 6 , IConst 10]
                              ,[IConst 5 , IConst 0, IConst 11, IConst 6 ]]))

-- Haskell array to MiniZinc array
translate :: [Int] -> Expr
translate [] = ArrayLit []
translate ls = ArrayLit $ map IConst ls

regularPlus :: Expr -> Item
regularPlus al = Constraint $ Call (userD "regular") [al, IConst 7, IConst 4, Var "trans_plus", IConst 1, SetLit [IConst 5, IConst 6, IConst 7]]

regularComb :: Expr -> Item
regularComb al = Constraint $ Call (userD "regular") [al, IConst 11, IConst 4, Var "trans_comb", IConst 1, SetLit [IConst 9, IConst 10, IConst 11]]

includeRegular :: Item
includeRegular = Include "\"regular.mzn\";"

varIdent :: Node -> Ident
varIdent z = "V_" ++ (show z)

propIdent :: Node -> Node -> Ident
propIdent z n = "Q" ++ (show z) ++ "_" ++ (show n)

declareVars :: [(Node, Maybe Sign, [(Node, Sign)], [(Node, Sign)])] -> [Item]
declareVars [] = []
declareVars ((z, Just s, outs, ins):rs)  = (Declare (Dec, Int) (varIdent z) (Just $ IConst (signToInt s))):((declareVars rs) ++ [Declare (Dec, Int) (propIdent z n) Nothing | (n, ls) <- (outs ++ ins)])
declareVars ((z, Nothing, outs, ins):rs) = (Declare (Dec, Int) (varIdent z) Nothing):((declareVars rs) ++ [Declare (Dec, Int) (propIdent z n) Nothing | (n, ls) <- (outs ++ ins)])

-- Node z is observed
constraint2 :: Node -> Sign -> [(Node, Sign)] -> [(Node, Sign)] -> [Item]
constraint2 z s outs ins = [regularComb $ ArrayLit [Var $ varIdent z, IConst $ signToInt ls, Var $ propIdent z n] | (n, ls) <- (outs ++ ins)]

-- Node z not observed
constraint1a :: Node -> [(Node, Sign)] -> [(Node, Sign)] -> [Item]
constraint1a z outs ins = [regularComb $ ArrayLit ([Var $ propIdent y z | (y,s2) <- delete (x, s1) (ins ++ outs)] ++ [IConst $ signToInt s1, Var $ propIdent z x]) | (x, s1) <- outs] ++
                          [regularComb $ ArrayLit ([Var $ propIdent y z | (y,s2) <- outs] ++ [IConst $ signToInt s1, Var $ propIdent z x]) | (x, s1) <- ins]

constraint1b :: Node -> [(Node, Sign)] -> [(Node, Sign)] -> [Item]
constraint1b z outs ins = [regularPlus $ ArrayLit ([Var $ propIdent x z | (x,s) <- (outs ++ ins)] ++ [Var $ varIdent z])]

makePost :: (Node, Maybe Sign, [(Node, Sign)], [(Node, Sign)]) -> [Item]
makePost (z, Just s, outs, ins) = constraint2 z s outs ins
makePost (z, Nothing, outs, ins) = (constraint1a z outs ins) ++ (constraint1b z outs ins)

makeModel :: [(Node, Maybe Sign, [(Node, Sign)], [(Node, Sign)])] -> MZModel
makeModel cld@(l:ls) = [includeRegular, Empty] ++
                       (declareVars cld) ++ 
                       [Empty, transitionPlus, Empty, transitionComb, Empty] ++ 
                       concat (map makePost cld) ++ [Empty, Solve Satisfy]
                       
iSolveCLD cld = iTestModel $ makeModel (getNodeContexts $ compile cld)

solveCLD graph mod dat = testModel (makeModel (getNodeContexts $ compile graph)) mod dat
