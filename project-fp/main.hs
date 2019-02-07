import Data.List
--import Debug.Trace

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
                         ++ (concat $ intersperse (" " ++ kind expr ++ " ") $ map show $ operands expr) 
                         ++ ")"

instance Ord ExprTree where
    compare (Const a) (Const b)   = compare a b
    compare (Var a) (Var b)       = compare a b
    compare (Const _) _           = LT
    compare _ (Const _)           = GT
    compare (Var _) _             = LT
    compare _ (Var _)             = GT
    compare (Add _) (Mult _)      = GT
    compare (Mult _) (Add _)      = LT
    compare (Pow x _) (Pow y _) = compare x y
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

combineWith :: String -> ExprTree -> ExprTree -> ExprTree
combineWith "*" expr1@(Pow x1 p1) expr2@(Pow x2 p2) = if x1 == x2
                                                      then (Pow x1 (Add [p1, p2]))
                                                      else (Mult [expr1, expr2])
combineWith "*" expr1@(Mult [Const a, p1@(Pow x1 _)]) expr2@(Mult [Const b, p2@(Pow x2 _)]) = 
    if x1 == x2
    then Mult [Mult [Const a, Const b], combineWith "*" p1 p2]
    else Mult [expr1, expr2]
combineWith "+" expr1@(Mult opLst1) expr2@(Mult opLst2) = if null commonVars 
                                                          then (Add [expr1, expr2])
                                                          else (Mult [addExpr, commonExpr])
    where
        nonConstants = filter (not . isConstant) 
        remove expr  = filter (/=expr)
        commonVars   = sort $ intersect (nonConstants opLst1) (nonConstants opLst2)
        commonExpr   = head $ commonVars
        addExpr      = Add ((remove commonExpr opLst1) ++ (remove commonExpr opLst2))
combineWith _ _ _ = error "Not supported combination"

combineListWith :: String -> [ExprTree] -> [ExprTree]
combineListWith _ []      = []
combineListWith opKind exprs = if opKind == "+" || opKind == "*" 
                               then combined : combineListWith opKind rest
                               else exprs
    where
        first = head exprs -- get first expression
        (alike, rest) = partition (alikeWith opKind first) exprs
        combined = foldl (combineWith opKind) first $ tail alike

multiply :: ExprTree -> ExprTree -> ExprTree
multiply (Const 0) _                         = Const 0
multiply (Const 1) expr                      = expr
multiply (Const x) (Div (Const p) (Const q)) = 
    reduceFraction $ Div (Const $ p * x) (Const q)  
multiply (Div (Const p1) (Const q1)) (Div (Const p2) (Const q2)) = 
    reduceFraction $ Div (Const $ p1 * p2) (Const $ q1 * q2)
multiply expr1@(Pow b1 p1) expr2@(Pow b2 p2) = if b1 == b2                      -- x^2 * x^10 = x^(2 + 10)
                                                then Pow b1 (Add [p1, p2])
                                                else Mult [expr1, expr2]
multiply expr1 expr2@(Pow base p)            = if expr1 == base                  -- x * x^2 = x^(2+1)
                                                then Pow expr1 $ Add [p, Const 1]
                                                else Mult [expr1, expr2]
multiply expr1 expr2                         = if expr1 == expr2 
                                                then (Pow expr1 (Const 2)) 
                                                else Mult [expr1, expr2]

add :: ExprTree -> ExprTree -> ExprTree
add (Const 0) expr = expr
add (Div (Const p1) (Const q1)) (Div (Const p2) (Const q2)) = (Div (Add [a,b]) (Const l))
    where 
        l = lcm q1 q2
        a = Const $ p1 * (l `div` q1)
        b = Const $ p2 * (l `div` q2)
add _ _ = Const 3


-- Removes unnecessary expressions
compress :: ExprTree -> ExprTree
compress (Pow _ (Const 0))   = Const 1
compress (Pow (Const x) y)   = if x == 1 || x == 0 then Const x else Pow (Const x) $ compress y
compress (Div x (Const 1))   = x
compress (Div x y)           = reduceFraction (Div x y)
compress (Mult opLst)        = Mult $ map compress $ (combineListWith "*" powers ++ rest)
    where
        (powers, rest) = partition isPower opLst
        isPower (Mult [Const _, Pow _ _]) = True
        isPower _                         = False
        
compress (Add opLst)       = Add $ map compress $ (combineListWith "+" mults ++ rest)
    where 
        (mults, rest) = partition ((=="*") . kind) opLst
compress expr              = expr


-- Returns True when expressions can be combined in
-- terms of some operation
alikeWith :: String -> ExprTree -> ExprTree -> Bool
alikeWith "*" (Pow x2 _) (Pow x1 _)                     = x1 == x2
alikeWith "*" (Mult [Const _, p1]) (Mult [Const _, p2]) = alikeWith "*" p1 p2
alikeWith "+" (Mult opLst1) (Mult opLst2) = not $ null commonVars -- can be combined if there is a common
    where                                                         -- non constant subexpression
        nonConstants = filter (not . isConstant) 
        commonVars   = sort $ intersect (nonConstants opLst1) (nonConstants opLst2)
alikeWith _ _ _      = False


-- For now only reduces rational numbers
reduceFraction :: ExprTree -> ExprTree
reduceFraction (Div (Const a) (Const b)) = 
    let k = gcd a b
    in if (b `div` k) == 1 
        then Const $ a `div` k 
        else Div (Const $ a `div` k) (Const $ b `div` k)
reduceFraction exprTree = exprTree

-- simple operation for expanding parentheses



e :: ExprTree
e = (Add [(Mult [(Mult [(Const 2), (Var 'x')]), (Mult [(Const 3), (Var 'x')])]), (Add [(Const 3), (Add [(Const 7), (Const 9)])])])

e1 :: ExprTree
e1 = (Mult [(Pow (Var 'x') (Const 4)), (Pow (Var 'y') (Const $ -1)), (Pow (Var 'y') (Const 3)), (Pow (Var 'x') (Const 2)), (Pow (Const 3) (Const 2))])

e2 :: ExprTree
e2 = (Add [Mult [Var 'x', Var 'x'], Var 'x', Var 'x'])
