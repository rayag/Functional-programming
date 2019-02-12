module ExprTree where
import Data.List
import Debug.Trace

data ExprTree =
    Const Int
    | Var Char
    | Add [ExprTree]
    | Mult [ExprTree]
    | Sub ExprTree ExprTree
    | Div ExprTree ExprTree 
    | Pow ExprTree ExprTree deriving Eq


instance Show ExprTree where
    show (Const x)     = if x >= 0 
                         then show x
                         else "(" ++ show x ++ ")"
    show (Var x)       = [x]
    show (Pow op1 op2) = "(" ++ show op1 ++ "^" ++ show op2 ++ ")"
    show expr          = "(" 
                         ++ (concat $ intersperse (" " 
                         ++ kind expr 
                         ++ " ") $ map show $ operands expr) 
                         ++ ")"

-- this is later used in expression reordering
instance Ord ExprTree where
    compare (Const a) (Const b)   = compare a b -- constants are ordered in ascending order
    compare (Var a) (Var b)       = compare a b -- characters according to the lexicographical order
    compare (Const _) _           = LT          -- constants are unconditionally precceding anything else
    compare _ (Const _)           = GT          
    compare (Var _) _             = LT
    compare _ (Var _)             = GT
    compare (Add _) (Mult _)      = GT
    compare (Mult _) (Add _)      = LT
    compare (Pow x p) (Pow y q)   = if x == y 
                                    then compare p q   
                                    else compare x y
    compare _ _                   = EQ

-- Returns the type of the operation as expressed by a string
kind :: ExprTree -> String
kind (Var _)   = "var"
kind (Add _)   = "+"
kind (Mult _)  = "*"
kind (Sub _ _) = "-"
kind (Div _ _) = "/"
kind (Pow _ _) = "^"
kind (Const _) = "const"

-- Returns operands of an expression
-- Example; operands $ Add [Const 1, Var 'x'] -> [Const 1, Var 'x']
--          operands $ Const 42               -> [Const 42]
operands :: ExprTree -> [ExprTree]
operands (Const a)     = [Const a]
operands (Var x)       = [Var x]
operands (Add opList)  = opList
operands (Mult opList) = opList
operands (Div op1 op2) = [op1, op2]
operands (Sub op1 op2) = [op1, op2]
operands (Pow op1 op2) = [op1, op2]

-- lowers the height of the tree
-- applicable for multiplication and addition
levelUp :: ExprTree -> ExprTree 
levelUp (Add opLst) = 
    let (additions, other) = partition ((=="+") . kind) opLst
        newOperands        = concat $ map operands additions
    in if null additions
        then (Add $ map levelUp opLst)
        else levelUp (Add $ map levelUp (other ++ newOperands)) -- repeat this until there is nothing to be levelled up
levelUp (Mult opLst) = 
    let (mults, other) = partition ((=="*") . kind) opLst
        newOperands    = concat $ map operands mults
    in if null mults
        then (Mult $ map levelUp opLst)
        else levelUp (Mult $ map levelUp (other ++ newOperands))
levelUp expr = expr
        

-- For now just moves constant in front of everything else
reorder :: ExprTree -> ExprTree
reorder expr = case expr of (Mult opLst) -> Mult $ sort $ map reorder opLst
                            (Add opLst)  -> Add $ sort $ map reorder opLst
                            _            -> expr  -- do not reorder const, vars and non commutative operations

-- Assigns operands to given operation
assignOperands :: String -> [ExprTree] -> Maybe ExprTree
assignOperands _ [] = Nothing
assignOperands "*" opLst = Just $ Mult opLst 
assignOperands "+" opLst = Just $ Add opLst
assignOperands "/" opLst 
    | (length opLst) < 2 || (opLst !! 1) == (Const 0) = Nothing
    | otherwise = Just $ Div (opLst !! 0) (opLst !! 1)
assignOperands "^" opLst
    | (length opLst) >= 2 = Just $ Pow (opLst !! 0) (opLst !! 1)
    | otherwise           = Nothing
assignOperands _ _       = Nothing

-- All expressions must have alike structure
-- so that further simplification would be easier
-- Example: x + 2 * x -> 1 * x + 2 * x
-- Works only on single variables for now
makeCanonical :: ExprTree -> ExprTree
makeCanonical expr@(Mult [x, p@(Pow (Var _) (Const _))]) = if isConstant x 
                                                           then expr -- it is in cannonical form 
                                                           else Mult [makeCanonical x, Mult [(Const 1), p]]
makeCanonical v@(Var _)    = Mult [Const 1, Pow v (Const 1)]
makeCanonical (Add opLst)  = (Add $ map makeCanonical opLst)
makeCanonical (Mult opLst) = (Mult $ map makeCanonical opLst) 
makeCanonical expr        = expr

-- Remove non commutative operations (those which can be removed of course)
-- subtraction -> x - y = x + (-1) * y
-- division    -> x / y = x * y ^(-1)
--             -> x / 2 = (1/2) * x
transformNegatives :: ExprTree -> ExprTree
transformNegatives (Sub (Const a) op)  = Add [Const $ -a, transformNegatives op]
transformNegatives (Sub op (Const a))  = Add [Const $ -a, transformNegatives op]
transformNegatives (Sub op1 op2)       = Add [newOp1, Mult [(Const (-1)), newOp2]]
    where newOp1 = transformNegatives op1
          newOp2 = transformNegatives op2
transformNegatives (Div op1 (Const a)) = Mult [Div (Const 1) (Const a), newOp]
    where newOp = transformNegatives op1
transformNegatives (Div op1 op2)       = Mult [op1, Pow op2 $ Const $ -1]
transformNegatives exprTree            = exprTree

isConstant :: ExprTree -> Bool
isConstant (Const _) = True
isConstant (Var _)   = False
isConstant expr      = all isConstant $ operands expr

getOperandAt :: Int -> ExprTree -> Maybe ExprTree
getOperandAt ind expr = if ind >= (length $ operands expr)
                        then Nothing
                        else Just $ operands expr !! ind

-- Simple function to multiply 2 expressions
multiply :: ExprTree -> ExprTree -> ExprTree
multiply (Const 0) _                         = Const 0
multiply (Const 1) expr                      = expr
multiply (Const x) (Const y)                 = Const $ x * y
multiply (Const x) (Div (Const p) (Const q)) = 
    reduceFraction $ Div (Const $ p * x) (Const q)  
multiply expr1@(Div (Const _) (Const _)) expr2@(Const _) =
    multiply expr2 expr1  -- reduce to problem which we have a solution for
multiply (Div (Const p1) (Const q1)) (Div (Const p2) (Const q2)) = 
    reduceFraction $ Div (Const $ p1 * p2) (Const $ q1 * q2)
multiply expr1@(Pow b1 p1) expr2@(Pow b2 p2) = if b1 == b2                       -- x^2 * x^10 = x^(2 + 10)
                                                then Pow b1 (Add [p1, p2])
                                                else Mult [expr1, expr2]
multiply expr1 expr2@(Pow base p)            = if expr1 == base                  -- x * x^2 = x^(2+1)
                                                then Pow expr1 $ Add [p, Const 1]
                                                else Mult [expr1, expr2]
multiply expr1 expr2                         = if expr1 == expr2 
                                                then (Pow expr1 (Const 2)) 
                                                else Mult [expr1, expr2]

-- Function that multiplies a list of expressions
-- combining them when possible
multiplyList :: [ExprTree] -> [ExprTree]
multiplyList []  = [Const 1]
multiplyList lst = multiplied : multiplyList rest
    where
        (alike, rest) = partition (alikeWith "*" (head lst)) lst
        multiplied    = foldl multiply (Const 1) alike

-- Simple function to perform addition of
-- two expressions
add :: ExprTree -> ExprTree -> ExprTree
add (Const 0) expr                      = expr
add expr (Const 0)                      = expr
add (Const x) (Const y)                 = if x + y < 1000   -- leave only constants that are less than 1000
                                          then Const $ x + y
                                          else Add [Const x, Const y]
add (Const x) (Div (Const p) (Const q)) = reduceFraction $ Div (add (Const $ x * q) (Const p)) (Const q)
add (Div (Const p1) (Const q1)) (Div (Const p2) (Const q2)) = 
    reduceFraction $ Div (Const $ a + b) (Const l)
    where 
        l = lcm q1 q2
        a = p1 * (l `div` q1)
        b = p2 * (l `div` q2)
add expr1@(Pow (Const b) (Const q)) expr2 = if b ^ q < 1000  -- leave only constants less than 1000
                                            then add expr2 (Const $ b^q)
                                            else (Add [expr1, expr2])
add (Mult opLst1) (Mult opLst2) 
    | length commonVars == 1 = Mult [Add (removeCommonVar $ opLst1 ++ opLst2), head commonVars]
    where 
        commonVars = intersect (filter (not . isConstant) opLst1)
                               (filter (not . isConstant) opLst2) 
        removeCommonVar = filter (/=(head commonVars))
add expr1 expr2@(Mult opLst)
    | (length $ filter (==expr1) opLst) == 1 = Mult [Add (Const 1 : rest), expr1]
    | otherwise = Add [expr1, expr2]
    where 
        rest = filter (/=expr1) opLst
add expr1 expr2 = if expr1 == expr2
                   then Mult [Const 2, expr1]
                   else Add [expr1, expr2]

-- Performs addition over a list by 
-- partitioning alike elements and then
-- combining them
addList :: [ExprTree] -> [ExprTree]
addList []  = []
addList lst = combined : addList rest
    where
        (alike, rest) = partition (alikeWith "+" (head lst)) lst
        combined      = foldl add (Const 0) alike

-- Simple function powering two expressions 
-- one by another
power :: ExprTree -> ExprTree
power (Pow _ (Const 0)) = Const 1
power (Pow (Const 1) _) = Const 1
power (Pow (Const 0) _) = Const 0
power (Pow (Pow x a) b) = Pow x (Mult [a, b])
power expr              = expr

-- Removes unnecessary expressions
-- and combines alike terms
compress :: ExprTree -> ExprTree
compress (Mult (Const 0:_)) = Const 0
compress (Div x (Const 1))  = x
compress (Pow x y)          = power $ Pow (compress x) (compress y)
compress (Div x y)          = reduceFraction (Div x y)
compress (Add opLst)        = trace ("add: " ++ (show (Add opLst))) $ if length opLst == 1
                              then compress $ head opLst
                              else Add $ addList $ map compress opLst
compress (Mult opLst)        
    | length opLst == 1   = compress $ head opLst
    | length multLst == 1 = compress $ head $ multLst
    | otherwise           = trace ("mult :" ++ (show (Mult opLst)) )Mult multLst
    where 
        multLst = filter (/= Const 1) $ multiplyList $ map compress opLst
compress expr               = expr

getCommonDivisor :: [ExprTree] -> ExprTree
getCommonDivisor [] = Const 1
getCommonDivisor lst = foldl1 commonDiv lst

extractMultiplier :: ExprTree -> ExprTree -> ExprTree
extractMultiplier (Const 1) expr = expr
extractMultiplier (Const x) (Add opLst) = Mult [Const x, Add $ map (multiply (Div (Const 1) (Const x))) opLst]
extractMultiplier m@(Div (Const p) (Const q)) (Add opLst) = Mult [m, Add $ map (multiply (Div (Const q) (Const p))) opLst]
extractMultiplier _ expr2 = expr2
         
-- Returns common divisor of two expressions
-- common divisor can only be an integer
-- in the terms of this function. If common
-- divisor could be a fraction than this function
-- has no sense.
commonDiv :: ExprTree -> ExprTree -> ExprTree
commonDiv (Const p) (Const q)    = Const $ gcd p q 
commonDiv (Const p) (Mult opLst) = Const $ gcd p q 
    where 
        (Const q) = head $ multiplyList $ filter isConstant opLst -- Const 1 if list is empty
commonDiv (Mult opLst1) (Mult opLst2) = Const $ gcd p q
    where
        getConst = (head) . (multiplyList) . (filter isConstant)
        (Const p) = getConst opLst1
        (Const q) = getConst opLst2
commonDiv _ _ = Const 1
                


-- Returns True when expressions can be combined in
-- terms of some operation
alikeWith :: String -> ExprTree -> ExprTree -> Bool
alikeWith "*" (Pow x2 _) (Pow x1 _)                     = x1 == x2  -- powers can be combined when their bases are equal
alikeWith "*" (Mult [Const _, p1]) (Mult [Const _, p2]) = alikeWith "*" p1 p2  -- consts do not affect alikeness
alikeWith "*" expr (Pow base _)                         = expr == base         -- x and x^a can be combined
alikeWith "+" (Mult opLst1) (Mult opLst2)               = (all isConstant $ opLst1 ++ opLst2)  -- all are constants
                                                          || (not $ null commonVars) -- can be combined if there is a common
    where                                                                          -- non constant subexpression
        nonConstants = filter (not . isConstant) 
        commonVars   = sort $ intersect (nonConstants opLst1) (nonConstants opLst2)
alikeWith "+" expr1 (Mult opLst) = (length $ filter (==expr1) opLst) == 1  
alikeWith _ expr1 expr2                                 = if isConstant expr1 && isConstant expr2
                                                           then True
                                                           else expr1 == expr2


-- For now only reduces rational numbers
reduceFraction :: ExprTree -> ExprTree
reduceFraction (Div (Const a) (Const b)) = 
    let k = gcd a b
    in if (b `div` k) == 1 
        then Const $ a `div` k 
        else Div (Const $ a `div` k) (Const $ b `div` k)
reduceFraction exprTree = exprTree

-- Derivative function
derive :: ExprTree -> ExprTree
derive (Const _)         = Const 0
derive (Var _)           = Const 1
derive (Pow expr1 expr2) = Mult [expr2, Pow expr1 $ Sub expr2 (Const 1), derive expr1]
derive (Add opList)      = Add $ map derive opList
derive (Mult (x:y:[]))   = Add [Mult [derive x, y], Mult [x, derive y]] 
derive expr              = expr

simplify :: Int -> ExprTree -> ExprTree
simplify 0 expr = expr
simplify n expr = trace (show simplified) $ if simplified == expr
                then expr
                else simplify (n - 1) simplified
    where
        simplified = compress $ reorder $ levelUp $ transformNegatives expr


-- Function for expression expanding
expand :: ExprTree -> ExprTree
expand (Var x) = Var x 
expand (Mult [Const a, Add opLst]) = 
    Add $ map (\expr -> Mult [Const a, expr]) opLst  -- a * (x + b) -> (a * x) + (a * b)
expand (Add opLst) = Add $ map expand opLst          -- exprand all addends
expand (Pow (Mult opLst) p) = Mult $ map (\x -> Pow x p) opLst  -- (x1*x2*...*xn)^k -> x1^k * x2^k * ... * xn^k
expand (Pow (Add (x:y:[])) (Const k))   = Add newOperands  -- (x + y)^k
    where
        newOperands = map (\(a, (px, py)) -> Mult [Const a, Pow x (Const px), Pow y (Const py)]) 
                        $ zipWith (,) (pascalTriangle !! k) (generatePowers k)
expand expr = expr                            

-- Generates next line of the pascal triangle
-- given the previous one
generatePascal :: [Int] -> [[Int]]
generatePascal []     = []
generatePascal (x:[]) = generatePascal [x,x]
generatePascal xs     = xs : generatePascal ([1] ++ takeSums xs ++ [1])
    where
        takeSums []     = []
        takeSums (_:[]) = []
        takeSums (y:ys) = (y + (head ys)) : takeSums ys

pascalTriangle :: [[Int]]
pascalTriangle  = [1,1] : generatePascal [1,1]

generatePowers :: Int -> [(Int, Int)]
generatePowers n = [(x,n-x) | x <- [0..n]]

e2 :: ExprTree
e2 = Pow (Add [Var 'x', Var 'y']) (Const 2)

e3 :: ExprTree
e3 = Pow (Add [Var 'x', Const 2]) (Const 3)

e :: ExprTree
e = Add [Mult [Var 'x', Const 2], Const 2]
import Data.List
import Debug.Trace

data ExprTree =
    Const Int
    | Var Char
    | Add [ExprTree]
    | Mult [ExprTree]
    | Sub ExprTree ExprTree
    | Div ExprTree ExprTree 
    | Pow ExprTree ExprTree deriving Eq


instance Show ExprTree where
    show (Const x)     = if x >= 0 
                         then show x
                         else "(" ++ show x ++ ")"
    show (Var x)       = [x]
    show (Pow op1 op2) = "(" ++ show op1 ++ "^" ++ show op2 ++ ")"
    show expr          = "(" 
                         ++ (concat $ intersperse (" " 
                         ++ kind expr 
                         ++ " ") $ map show $ operands expr) 
                         ++ ")"

-- this is later used in expression reordering
instance Ord ExprTree where
    compare (Const a) (Const b)   = compare a b -- constants are ordered in ascending order
    compare (Var a) (Var b)       = compare a b -- characters according to the lexicographical order
    compare (Const _) _           = LT          -- constants are unconditionally precceding anything else
    compare _ (Const _)           = GT          
    compare (Var _) _             = LT
    compare _ (Var _)             = GT
    compare (Add _) (Mult _)      = GT
    compare (Mult _) (Add _)      = LT
    compare (Pow x p) (Pow y q)   = if x == y 
                                    then compare p q   
                                    else compare x y
    compare _ _                   = EQ

-- Returns the type of the operation as expressed by a string
kind :: ExprTree -> String
kind (Var _)   = "var"
kind (Add _)   = "+"
kind (Mult _)  = "*"
kind (Sub _ _) = "-"
kind (Div _ _) = "/"
kind (Pow _ _) = "^"
kind (Const _) = "const"

-- Returns operands of an expression
-- Example; operands $ Add [Const 1, Var 'x'] -> [Const 1, Var 'x']
--          operands $ Const 42               -> [Const 42]
operands :: ExprTree -> [ExprTree]
operands (Const a)     = [Const a]
operands (Var x)       = [Var x]
operands (Add opList)  = opList
operands (Mult opList) = opList
operands (Div op1 op2) = [op1, op2]
operands (Sub op1 op2) = [op1, op2]
operands (Pow op1 op2) = [op1, op2]

-- lowers the height of the tree
-- applicable for multiplication and addition
levelUp :: ExprTree -> ExprTree 
levelUp (Add opLst) = 
    let (additions, other) = partition ((=="+") . kind) opLst
        newOperands        = concat $ map operands additions
    in if null additions
        then (Add $ map levelUp opLst)
        else levelUp (Add $ map levelUp (other ++ newOperands)) -- repeat this until there is nothing to be levelled up
levelUp (Mult opLst) = 
    let (mults, other) = partition ((=="*") . kind) opLst
        newOperands    = concat $ map operands mults
    in if null mults
        then (Mult $ map levelUp opLst)
        else levelUp (Mult $ map levelUp (other ++ newOperands))
levelUp expr = expr
        

-- For now just moves constant in front of everything else
reorder :: ExprTree -> ExprTree
reorder expr = case expr of (Mult opLst) -> Mult $ sort $ map reorder opLst
                            (Add opLst)  -> Add $ sort $ map reorder opLst
                            _            -> expr  -- do not reorder const, vars and non commutative operations

-- Assigns operands to given operation
assignOperands :: String -> [ExprTree] -> Maybe ExprTree
assignOperands _ [] = Nothing
assignOperands "*" opLst = Just $ Mult opLst 
assignOperands "+" opLst = Just $ Add opLst
assignOperands "/" opLst 
    | (length opLst) < 2 || (opLst !! 1) == (Const 0) = Nothing
    | otherwise = Just $ Div (opLst !! 0) (opLst !! 1)
assignOperands "^" opLst
    | (length opLst) >= 2 = Just $ Pow (opLst !! 0) (opLst !! 1)
    | otherwise           = Nothing
assignOperands _ _       = Nothing

-- All expressions must have alike structure
-- so that further simplification would be easier
-- Example: x + 2 * x -> 1 * x + 2 * x
-- Works only on single variables for now
makeCanonical :: ExprTree -> ExprTree
makeCanonical expr@(Mult [x, p@(Pow (Var _) (Const _))]) = if isConstant x 
                                                           then expr -- it is in cannonical form 
                                                           else Mult [makeCanonical x, Mult [(Const 1), p]]
makeCanonical v@(Var _)    = Mult [Const 1, Pow v (Const 1)]
makeCanonical (Add opLst)  = (Add $ map makeCanonical opLst)
makeCanonical (Mult opLst) = (Mult $ map makeCanonical opLst) 
makeCanonical expr        = expr

-- Remove non commutative operations (those which can be removed of course)
-- subtraction -> x - y = x + (-1) * y
-- division    -> x / y = x * y ^(-1)
--             -> x / 2 = (1/2) * x
transformNegatives :: ExprTree -> ExprTree
transformNegatives (Sub (Const a) op)  = Add [Const $ -a, transformNegatives op]
transformNegatives (Sub op (Const a))  = Add [Const $ -a, transformNegatives op]
transformNegatives (Sub op1 op2)       = Add [newOp1, Mult [(Const (-1)), newOp2]]
    where newOp1 = transformNegatives op1
          newOp2 = transformNegatives op2
transformNegatives (Div op1 (Const a)) = Mult [Div (Const 1) (Const a), newOp]
    where newOp = transformNegatives op1
transformNegatives (Div op1 op2)       = Mult [op1, Pow op2 $ Const $ -1]
transformNegatives exprTree            = exprTree

isConstant :: ExprTree -> Bool
isConstant (Const _) = True
isConstant (Var _)   = False
isConstant expr      = all isConstant $ operands expr

getOperandAt :: Int -> ExprTree -> Maybe ExprTree
getOperandAt ind expr = if ind >= (length $ operands expr)
                        then Nothing
                        else Just $ operands expr !! ind

-- Simple function to multiply 2 expressions
multiply :: ExprTree -> ExprTree -> ExprTree
multiply (Const 0) _                         = Const 0
multiply (Const 1) expr                      = expr
multiply (Const x) (Const y)                 = Const $ x * y
multiply (Const x) (Div (Const p) (Const q)) = 
    reduceFraction $ Div (Const $ p * x) (Const q)  
multiply expr1@(Div (Const _) (Const _)) expr2@(Const _) =
    multiply expr2 expr1  -- reduce to problem which we have a solution for
multiply (Div (Const p1) (Const q1)) (Div (Const p2) (Const q2)) = 
    reduceFraction $ Div (Const $ p1 * p2) (Const $ q1 * q2)
multiply expr1@(Pow b1 p1) expr2@(Pow b2 p2) = if b1 == b2                       -- x^2 * x^10 = x^(2 + 10)
                                                then Pow b1 (Add [p1, p2])
                                                else Mult [expr1, expr2]
multiply expr1 expr2@(Pow base p)            = if expr1 == base                  -- x * x^2 = x^(2+1)
                                                then Pow expr1 $ Add [p, Const 1]
                                                else Mult [expr1, expr2]
multiply expr1 expr2                         = if expr1 == expr2 
                                                then (Pow expr1 (Const 2)) 
                                                else Mult [expr1, expr2]

-- Function that multiplies a list of expressions
-- combining them when possible
multiplyList :: [ExprTree] -> [ExprTree]
multiplyList []  = [Const 1]
multiplyList lst = multiplied : multiplyList rest
    where
        (alike, rest) = partition (alikeWith "*" (head lst)) lst
        multiplied    = foldl multiply (Const 1) alike

-- Simple function to perform addition of
-- two expressions
add :: ExprTree -> ExprTree -> ExprTree
add (Const 0) expr                      = expr
add expr (Const 0)                      = expr
add (Const x) (Const y)                 = if x + y < 1000   -- leave only constants that are less than 1000
                                          then Const $ x + y
                                          else Add [Const x, Const y]
add (Const x) (Div (Const p) (Const q)) = reduceFraction $ Div (add (Const $ x * q) (Const p)) (Const q)
add (Div (Const p1) (Const q1)) (Div (Const p2) (Const q2)) = 
    reduceFraction $ Div (Const $ a + b) (Const l)
    where 
        l = lcm q1 q2
        a = p1 * (l `div` q1)
        b = p2 * (l `div` q2)
add expr1@(Pow (Const b) (Const q)) expr2 = if b ^ q < 1000  -- leave only constants less than 1000
                                            then add expr2 (Const $ b^q)
                                            else (Add [expr1, expr2])
add (Mult opLst1) (Mult opLst2) 
    | length commonVars == 1 = Mult [Add (removeCommonVar $ opLst1 ++ opLst2), head commonVars]
    where 
        commonVars = intersect (filter (not . isConstant) opLst1)
                               (filter (not . isConstant) opLst2) 
        removeCommonVar = filter (/=(head commonVars))
add expr1 expr2@(Mult opLst)
    | (length $ filter (==expr1) opLst) == 1 = Mult [Add (Const 1 : rest), expr1]
    | otherwise = Add [expr1, expr2]
    where 
        rest = filter (/=expr1) opLst
add expr1 expr2 = if expr1 == expr2
                   then Mult [Const 2, expr1]
                   else Add [expr1, expr2]

-- Performs addition over a list by 
-- partitioning alike elements and then
-- combining them
addList :: [ExprTree] -> [ExprTree]
addList []  = []
addList lst = combined : addList rest
    where
        (alike, rest) = partition (alikeWith "+" (head lst)) lst
        combined      = foldl add (Const 0) alike

-- Simple function powering two expressions 
-- one by another
power :: ExprTree -> ExprTree
power (Pow _ (Const 0)) = Const 1
power (Pow (Const 1) _) = Const 1
power (Pow (Const 0) _) = Const 0
power (Pow (Pow x a) b) = Pow x (Mult [a, b])
power expr              = expr

-- Removes unnecessary expressions
-- and combines alike terms
compress :: ExprTree -> ExprTree
compress (Mult (Const 0:_)) = Const 0
compress (Div x (Const 1))  = x
compress (Pow x y)          = power $ Pow (compress x) (compress y)
compress (Div x y)          = reduceFraction (Div x y)
compress (Add opLst)        = if length opLst == 1
                              then compress $ head opLst
                              else Add $ addList $ map compress opLst
compress (Mult opLst)        
    | length opLst == 1   = compress $ head opLst
    | length multLst == 1 = compress $ head $ multLst
    | otherwise           = Mult multLst
    where 
        multLst = filter (/= Const 1) $ multiplyList $ map compress opLst
compress expr               = expr

getCommonDivisor :: [ExprTree] -> ExprTree
getCommonDivisor [] = Const 1
getCommonDivisor lst = foldl1 commonDiv lst

extractMultiplier :: ExprTree -> ExprTree -> ExprTree
extractMultiplier (Const 1) expr = expr
extractMultiplier (Const x) (Add opLst) = Mult [Const x, Add $ map (multiply (Div (Const 1) (Const x))) opLst]
extractMultiplier m@(Div (Const p) (Const q)) (Add opLst) = Mult [m, Add $ map (multiply (Div (Const q) (Const p))) opLst]
extractMultiplier _ expr2 = expr2
         
-- Returns common divisor of two expressions
-- common divisor can only be an integer
-- in the terms of this function. If common
-- divisor could be a fraction than this function
-- has no sense.
commonDiv :: ExprTree -> ExprTree -> ExprTree
commonDiv (Const p) (Const q)    = Const $ gcd p q 
commonDiv (Const p) (Mult opLst) = Const $ gcd p q 
    where 
        (Const q) = head $ multiplyList $ filter isConstant opLst -- Const 1 if list is empty
commonDiv (Mult opLst1) (Mult opLst2) = Const $ gcd p q
    where
        getConst = (head) . (multiplyList) . (filter isConstant)
        (Const p) = getConst opLst1
        (Const q) = getConst opLst2
commonDiv _ _ = Const 1
                


-- Returns True when expressions can be combined in
-- terms of some operation
alikeWith :: String -> ExprTree -> ExprTree -> Bool
alikeWith "*" (Pow x2 _) (Pow x1 _)                     = x1 == x2  -- powers can be combined when their bases are equal
alikeWith "*" (Mult [Const _, p1]) (Mult [Const _, p2]) = alikeWith "*" p1 p2  -- consts do not affect alikeness
alikeWith "*" expr (Pow base _)                         = expr == base         -- x and x^a can be combined
alikeWith "+" (Mult opLst1) (Mult opLst2)               = (all isConstant $ opLst1 ++ opLst2)  -- all are constants
                                                          || (not $ null commonVars) -- can be combined if there is a common
    where                                                                          -- non constant subexpression
        nonConstants = filter (not . isConstant) 
        commonVars   = sort $ intersect (nonConstants opLst1) (nonConstants opLst2)
alikeWith "+" expr1 (Mult opLst) = (length $ filter (==expr1) opLst) == 1  
alikeWith _ expr1 expr2                                 = if isConstant expr1 && isConstant expr2
                                                           then True
                                                           else expr1 == expr2


-- For now only reduces rational numbers
reduceFraction :: ExprTree -> ExprTree
reduceFraction (Div (Const a) (Const b)) = 
    let k = gcd a b
    in if (b `div` k) == 1 
        then Const $ a `div` k 
        else Div (Const $ a `div` k) (Const $ b `div` k)
reduceFraction exprTree = exprTree

-- Derivative function
derive :: ExprTree -> ExprTree
derive (Const _)         = Const 0
derive (Var _)           = Const 1
derive (Pow expr1 expr2) = Mult [expr2, Pow expr1 $ Sub expr2 (Const 1), derive expr1]
derive (Add opList)      = Add $ map derive opList
derive (Mult (x:y:[]))   = Add [Mult [derive x, y], Mult [x, derive y]] 
derive expr              = expr

simplify :: Int -> ExprTree -> ExprTree
simplify 0 expr = expr
simplify n expr = trace (show simplified) $ if simplified == expr
                then expr
                else simplify (n - 1) simplified
    where
        simplified = compress $ reorder $ levelUp $ transformNegatives expr


-- Function for expression expanding
expand :: ExprTree -> ExprTree
expand (Var x) = Var x 
expand (Mult [Const a, Add opLst]) = 
    Add $ map (\expr -> Mult [Const a, expr]) opLst  -- a * (x + b) -> (a * x) + (a * b)
expand (Add opLst) = Add $ map expand opLst          -- exprand all addends
expand (Pow (Mult opLst) p) = Mult $ map (\x -> Pow x p) opLst  -- (x1*x2*...*xn)^k -> x1^k * x2^k * ... * xn^k
expand (Pow (Add (x:y:[])) (Const k))   = Add newOperands  -- (x + y)^k
    where
        newOperands = map (\(a, (px, py)) -> Mult [Const a, Pow x (Const px), Pow y (Const py)]) 
                        $ zipWith (,) (pascalTriangle !! k) (generatePowers k)
expand expr = expr                            

-- Generates next line of the pascal triangle
-- given the previous one
generatePascal :: [Int] -> [[Int]]
generatePascal []     = []
generatePascal (x:[]) = generatePascal [x,x]
generatePascal xs     = xs : generatePascal ([1] ++ takeSums xs ++ [1])
    where
        takeSums []     = []
        takeSums (_:[]) = []
        takeSums (y:ys) = (y + (head ys)) : takeSums ys

pascalTriangle :: [[Int]]
pascalTriangle  = [1,1] : generatePascal [1,1]

generatePowers :: Int -> [(Int, Int)]
generatePowers n = [(x,n-x) | x <- [0..n]]

e2 :: ExprTree
e2 = Pow (Add [Var 'x', Var 'y']) (Const 2)

e3 :: ExprTree
e3 = Pow (Add [Var 'x', Const 2]) (Const 3)

e :: ExprTree
e = Add [Mult [Var 'x', Const 2], Const 2]
