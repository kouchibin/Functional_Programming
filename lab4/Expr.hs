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

getFunByName :: String -> Maybe Func
getFunByName s | s == "sin" = Just Sin
               | s == "cos" = Just Cos
               | otherwise = Nothing

data Expr = Num Double |
            Var |
            Add Expr Expr |
            Mul Expr Expr |
            App Func  Expr
            deriving (Show, Eq)

x :: Expr
x = Var

num :: Double -> Expr
num d = Num d

add, mul :: Expr -> Expr -> Expr
add e1 e2 = Add e1 e2
mul e1 e2 = Mul e1 e2

sin, cos :: Expr -> Expr
sin e = App Sin e
cos e = App Cos e

--------------- B --------------- 

showFactor :: Expr -> String
showFactor (Add e1 e2) = "(" ++ showExpr (Add e1 e2) ++ ")"
showFactor e           = showExpr e

showExpr :: Expr -> String
showExpr (Num n) | n >= 0 = show n 
                 | otherwise = "(" ++ show n ++ ")"
showExpr Var     = "x"
showExpr (Add e1 (Add e2 e3)) = showExpr e1 ++ "+(" ++ showExpr (Add e2 e3) ++ ")" 
showExpr (Add e1 e2)          = showExpr e1 ++ "+" ++ showExpr e2 
showExpr (Mul e1 (Mul e2 e3)) = showFactor e1 ++ "*(" ++ showFactor (Mul e2 e3) ++ ")"
showExpr (Mul e1 e2) = showFactor e1 ++ "*" ++ showFactor e2

showExpr (App Sin (Num n)) = "sin " ++ show n
showExpr (App Sin (Var)) = "sin x" 
showExpr (App Sin e) = "sin(" ++ showExpr e ++ ")" 

showExpr (App Cos (Num n)) = "cos " ++ show n
showExpr (App Cos (Var)) = "cos x" 
showExpr (App Cos e) = "cos(" ++ showExpr e ++ ")"

--------------- C --------------- 
eval :: Expr -> Double -> Double 


eval (Num n) _     = n
eval Var     v     = v
eval (Add e1 e2) v = eval e1 v + eval e2 v
eval (Mul e1 e2) v = eval e1 v * eval e2 v
eval (App f e)   v = evalFun f $ eval e v

--------------- D --------------- 
number :: Parser Expr 
number = Num <$> readsP 

operator c op = do
    n <- number
    char c
    m <- number
    return (n `op` m)

funcName :: Parser Expr
funcName = do
    zeroOrMore $ char ' '
    name <- oneOrMore $ sat isLetter
    let func = getFunByName name
    e <- factor
    if (isNothing func) then failure
    else return (App (fromJust func) e)

varP :: Parser Expr
varP = do
    zeroOrMore $ char ' '
    char 'x'
    return Var

expr, term, factor :: Parser Expr
expr   = foldl1 Add <$> chain term (char '+')
term   = foldl1 Mul <$> chain factor (char '*')
factor = funcName <|> varP <|>  number  <|> char '(' *> expr <* char ')'  

readExpr :: String -> Maybe Expr
readExpr s = if (isNothing e) then Nothing
             else Just $ fst $ fromJust e
    where e = parse expr s

--------------- E --------------- 

prop_ShowReadExpr :: Expr -> Bool
-- prop_ShowReadExpr e = shownBefore == shownAfter
--                     where shownBefore = showExpr e
--                           shownAfter  = showExpr(fromJust (readExpr shownBefore))
prop_ShowReadExpr e = e == (fromJust $ readExpr $ showExpr e) 

arbExpr :: Int -> Gen Expr
arbExpr i = frequency [(1, rNum), (1, rVar), (i, rBin), (i, rApp)]
    where
        rNum = Num <$> arbitrary
        rVar = return Var
        rBin = do
            op <- elements [Add, Mul]
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
simplify (Num d) = Num d
simplify Var = Var

simplify (Add (Num d1) (Num d2)) = Num (d1 + d2)
simplify (Add (Num 0) e)         = e
simplify (Add e (Num 0))         = e
simplify (Add (Num d) Var) = Add (Num d) Var
simplify (Add Var (Num d)) = Add Var (Num d)
simplify (Add e1 e2)             =  case e1' of 
        Num e1n -> case e2' of
                    Num e2n -> simplify $ Add e1' e2'
                    _       -> Add e1' e2'
        _       -> Add e1' e2'
    where e1' = simplify e1
          e2' = simplify e2

simplify (Mul (Num d1) (Num d2)) = Num (d1 * d2)
simplify (Mul (Num 0) e)         = Num 0
simplify (Mul e (Num 0))         = Num 0
simplify (Mul (Num 1) e)         = e
simplify (Mul e (Num 1))         = e
simplify (Mul (Num d) Var) = Mul (Num d) Var
simplify (Mul Var (Num d)) = Mul Var (Num d)
simplify (Mul e1 e2)             = case e1' of 
        Num e1n -> case e2' of
                    Num e2n -> simplify $ Mul e1' e2'
                    _       -> Mul e1' e2'
        _       -> Mul e1' e2'
    where e1' = simplify e1
          e2' = simplify e2

simplify (App f e)               = App f (simplify e)

prop_simplify :: Expr -> Double -> Bool 
prop_simplify e x =  eval e x == eval se x && simplify se == se  
    where se = simplify e


--------------- G --------------- 

differentiate :: Expr -> Expr
differentiate (Num d) = Num 0
differentiate Var     = Num 1
differentiate (Add e1 e2) = simplify (Add (differentiate e1) (differentiate e2))
differentiate (Mul e1 e2) = simplify (Add (Mul (differentiate e1) e2) (Mul (differentiate e2) e1))
differentiate (App Sin e) = simplify(Mul(differentiate e) (App Cos e))
differentiate (App Cos e) = simplify(Mul (Num (-1)) (Mul(differentiate e) (App Sin e)))

