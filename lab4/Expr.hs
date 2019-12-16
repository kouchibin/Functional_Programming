module Expr where

import Data.Char
import Data.Maybe
import Parsing
import Test.QuickCheck

--------------- A --------------- 

-- New functions can be added here.
data Func = Sin | Cos
    deriving (Read, Show, Eq)

evalFun :: Func -> (Double -> Double)
evalFun Sin = Prelude.sin
evalFun Cos = Prelude.cos

funStr :: Func -> String
funStr Sin = "sin"
funStr Cos = "cos"

-- A dictionary can be used when there are more functions.
getFunByName :: String -> Maybe Func
getFunByName s | s == "sin" = Just Sin
               | s == "cos" = Just Cos
               | otherwise = Nothing

data Expr = Num Double |
            Var |
            Bin Expr Char Expr |
            App Func Expr
            deriving (Read, Show, Eq)

x :: Expr
x = Var

num :: Double -> Expr
num d = Num d

add, mul :: Expr -> Expr -> Expr
add e1 e2 = Bin e1 '+' e2
mul e1 e2 = Bin e1 '*' e2

sin, cos :: Expr -> Expr
sin e = App Sin e
cos e = App Cos e

-- We thought about using the operator function inside the definition of Expr,
-- but then we cannot derive Eq, which is needed when comparing Exprs.
getOp :: Char -> (Double -> Double -> Double)
getOp '+' = (+)
getOp '*' = (*)

--------------- B --------------- 

showFactor :: Expr -> String
showFactor (Bin e1 '+' e2) = "(" ++ showExpr (add e1 e2) ++ ")"
showFactor e               = showExpr e

showExpr :: Expr -> String
showExpr (Num n) | n >= 0    = show n 
                 | otherwise = "(" ++ show n ++ ")"
showExpr Var                 = "x"

showExpr (Bin e1 opl (Bin e2 opr e3)) 
    | opl == opr && opl == '*'  = showFactor e1 ++ 
                                  (opl:"(") ++ 
                                  showFactor (Bin e2 opr e3) ++ 
                                  ")" 
    | opl == opr                = showExpr e1 ++ 
                                  (opl:"(") ++ 
                                  showExpr (Bin e2 opr e3) ++ 
                                  ")" 
showExpr (Bin e1 '*' e2)        = showFactor e1 ++ "*" ++ showFactor e2
showExpr (Bin e1 op e2)         = showExpr e1 ++ [op] ++ showExpr e2
showExpr (App f (Bin e1 op e2)) = funStr f ++ "(" ++ showExpr (Bin e1 op e2) ++ ")" 
showExpr (App f e)              = funStr f ++ " " ++ showExpr e 

--------------- C --------------- 

eval :: Expr -> Double -> Double
eval (Num n) _     = n
eval Var     v     = v
eval (Bin e1 '+' e2) v = eval e1 v + eval e2 v
eval (Bin e1 '*' e2) v = eval e1 v * eval e2 v
eval (App f e)   v = evalFun f $ eval e v

--------------- D --------------- 

number :: Parser Expr 
number = Num <$> readsP 

skipSpace :: Parser ()
skipSpace = do
    zeroOrMore $ char ' '
    return ()

funcP :: Parser Expr
funcP = do
    skipSpace
    name <- oneOrMore $ sat isLetter
    let func = getFunByName name
    e <- factor
    skipSpace
    if (isNothing func) then failure
    else return (App (fromJust func) e)

varP :: Parser Expr
varP = do
    skipSpace
    char 'x'
    skipSpace
    return Var

paren :: Parser Expr
paren = do
    skipSpace
    char '('
    skipSpace
    exp <- expr
    skipSpace
    char ')'
    skipSpace
    return exp
    
expr, term, factor :: Parser Expr
expr   = foldl1 add <$> chain term (char '+')
term   = foldl1 mul <$> chain factor (char '*')
factor = funcP <|> varP <|> number <|> paren  

readExpr :: String -> Maybe Expr
readExpr s = if (isNothing e) then Nothing
             else Just $ fst $ fromJust e
    where e = parse expr s

--------------- E --------------- 

prop_ShowReadExpr :: Expr -> Bool
prop_ShowReadExpr e = e == (fromJust $ readExpr $ showExpr e) 

arbExpr :: Int -> Gen Expr
arbExpr i = frequency [(1, rNum), (1, rVar), (i, rBin), (i, rApp)]
    where
        rNum = Num <$> arbitrary
        rVar = return Var
        rBin = do
            op <- elements [add, mul]
            let i' = i `div` 2
            e1 <- arbExpr i'
            e2 <- arbExpr i'
            return $ op e1 e2
        rApp = do
            fn <- elements [Sin, Cos]
            let i' = i `div` 2
            e <- arbExpr i'
            return $ App fn e

instance Arbitrary Expr where
    arbitrary = sized arbExpr

--------------- F --------------- 

simplify :: Expr -> Expr
simplify (Num d)   = Num d
simplify Var       = Var
simplify (App f e) = App f (simplify e)

simplify (Bin (Num d1) op (Num d2)) = Num $ (getOp op) d1 d2
simplify (Bin (Num 0) '+' e)         = simplify e
simplify (Bin e '+' (Num 0))         = simplify e
simplify (Bin (Num 0) '*' e)         = Num 0
simplify (Bin e '*' (Num 0))         = Num 0
simplify (Bin (Num 1) '*' e)         = simplify e
simplify (Bin e '*' (Num 1))         = simplify e

-- Simplify "1 + (2 + x)" and similar cases to "3 + x".
-- Some copy and paste code here but there doesn't seem to be a good way to refactor this
simplify (Bin (Bin (Num d1) op1 e) op2 (Num d2)) 
    | op1 == op2 = simplify $ Bin (Num ((getOp op1) d1 d2)) op1 e
simplify (Bin (Bin e op1 (Num d1)) op2 (Num d2)) 
    | op1 == op2 = simplify $ Bin (Num ((getOp op1) d1 d2)) op1 e
simplify (Bin (Num d1) op1 (Bin e op2 (Num d2))) 
    | op1 == op2 = simplify $ Bin (Num ((getOp op1) d1 d2)) op1 e
simplify (Bin (Num d1) op1 (Bin (Num d2) op2 e)) 
    | op1 == op2 = simplify $ Bin (Num ((getOp op1) d1 d2)) op1 e

-- Need to prevent infinite recurisive call.
simplify (Bin e1 op e2) = if e1 == e1' && e2 == e2' 
                          then Bin e1' op e2'
                          else simplify $ Bin e1' op e2'
    where e1' = simplify e1
          e2' = simplify e2

prop_simplify :: Expr -> Double -> Property 
prop_simplify e x =  within timeout $ diff < epsilon && simplify se == se  
    where se = simplify e
          timeout = 1000000
          epsilon = 0.0001
          diff    = abs (eval e x - eval se x)

--------------- G --------------- 

differentiate :: Expr -> Expr
differentiate (Num d)     = Num 0
differentiate Var         = Num 1
differentiate (Bin e1 '+' e2) = simplify $ add (differentiate e1) 
                                               (differentiate e2)
differentiate (Bin e1 '*' e2) = simplify $ add (mul (differentiate e1) e2)
                                               (mul (differentiate e2) e1)
differentiate (App Sin e) = simplify (mul (differentiate e) (App Cos e))
differentiate (App Cos e) = simplify (mul (Num (-1)) 
                                          (mul (differentiate e) (App Sin e)))

