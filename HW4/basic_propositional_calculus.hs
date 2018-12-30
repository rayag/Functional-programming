module Homework (Name, Prop, Environment, varValue, allVars, evalWithEnv, bind, allEnvs, isTautology, isContradiction, isSatisfiable, isAxiom, proofFrom, allBools, modusPonens, semanticallyEquivalent, semanticallyImplies) where

type Name = String

data Prop = Const Bool
          | Var Name
          | Not Prop
          | And Prop Prop
          | Or Prop Prop
          | Implies Prop Prop

infixr 7 `Implies`
infixl 9 `And`
infixl 8 `Or`

instance Show Prop where 
    show (Const b)          = if b then "T" else "F"
    show (Var x)            = x
    show (Not fi)           = '¬' : show fi
    show (fi `And` psi)     = show fi ++ " & " ++ show psi
    show (fi `Or` psi)      = "(" ++ show fi ++ " v " ++ show psi ++ ")"
    show (fi `Implies` psi) = "(" ++ show fi ++ " -> "  ++ show psi ++ ")"

type Environment = [(Name, Bool)]

instance Eq Prop where
    (==) (Const b1) (Const b2)               = b1 == b2
    (==) (Var x1) (Var x2)                   = x1 == x2
    (==) (Not p1) (Not p2)                   = p1 == p2
    (==) (p1 `And` p2) (p3 `And` p4)         = p1 == p3 && p2 == p4
    (==) (p1 `Or` p2) (p3 `Or` p4)           = p1 == p3 && p2 == p4
    (==) (p1 `Implies` p2) (p3 `Implies` p4) = p1 == p3 && p2 == p4
    (==) _ _                                 = False

varValue :: Environment -> Name -> Bool
varValue [] _ = error "Empty environment"
varValue ((key, value):rest) name 
    | key == name = value
    | otherwise   = varValue rest name

evalWithEnv :: Environment -> Prop -> Bool
evalWithEnv _ (Const b)            = b
evalWithEnv env (Var fi)           = varValue env fi
evalWithEnv env (Not fi)           = not $ evalWithEnv env fi
evalWithEnv env (fi `And` psi)     = evalWithEnv env fi && evalWithEnv env psi
evalWithEnv env (fi `Or` psi)      = evalWithEnv env fi || evalWithEnv env psi
evalWithEnv env (fi `Implies` psi) = (evalWithEnv env fi && evalWithEnv env psi) || (evalWithEnv env (Not fi))

makeSet :: (Eq a) => [a] -> [a]
makeSet [] = []
makeSet (x:xs) = x : (makeSet (filter (/=x) xs))

allVars :: Prop -> [String]
allVars (Var x)            = [x]
allVars (Not fi)           = makeSet $ allVars fi
allVars (fi `And` psi)     = makeSet $ allVars fi ++ allVars psi
allVars (fi `Or` psi)      = makeSet $ allVars fi ++ allVars psi
allVars (fi `Implies` psi) = makeSet $ allVars fi ++ allVars psi
allVars _                  = []

bind :: [Name] -> [Bool] -> Environment
bind _ []          = []
bind [] _          = []
bind (n:ns) (b:bs) = (n, b) : bind ns bs

allBools :: Int -> [[Bool]]
allBools 0 = []
allBools 1 = [[True], [False]]
allBools n = map (False:) r ++ map (True:) r
    where r = allBools $ n - 1

allEnvs :: [Name] -> [Environment]
allEnvs names = map (bind names) (allBools $ length names)

isTautology :: Prop -> Bool
isTautology fi = all (`evalWithEnv` fi) $ allEnvs $ allVars fi

isSatisfiable :: Prop -> Bool
isSatisfiable fi = any (`evalWithEnv` fi) $ allEnvs $ allVars fi

isContradiction :: Prop -> Bool
isContradiction fi = not $ isSatisfiable fi

semanticallyImplies :: Prop -> Prop -> Bool
semanticallyImplies fi psi = all (`evalWithEnv` psi) $ filter (`evalWithEnv` fi) $ allPossibleEnvironments
    where allPossibleEnvironments = allEnvs $ makeSet $ allVars fi ++ allVars psi

semanticallyEquivalent :: Prop -> Prop -> Bool
semanticallyEquivalent fi psi = semanticallyImplies fi psi && semanticallyImplies psi fi

isAxiom :: Prop -> Bool
isAxiom ((fi1 `Implies` hi1 `Implies` psi1) `Implies` ((fi2 `Implies` hi2) `Implies` (fi3 `Implies` psi2))) = fi1 == fi2 && fi1 == fi3 && hi1 == hi2 && psi1 == psi2    -- THEN-2
isAxiom (fi1 `And` hi `Implies` fi2)                                                                        = fi1 == fi2 || hi == fi2                                   -- AND-1 AND-2
isAxiom (fi1 `Implies` (fi2 `Or` fi3))                                                                      = fi1 == fi2 || fi1 == fi3                                  -- OR-1, OR-2
isAxiom ((fi1 `Implies` psi1) `Implies` ((hi1 `Implies` psi2) `Implies` (fi2 `Or` hi2 `Implies` psi3)))     = fi1 == fi2 && psi1 == psi2 && psi1 == psi3 && hi1 == hi2  -- OR-3
isAxiom (fi1 `Implies` hi1 `Implies` ((fi2 `Implies` (Not hi2)) `Implies` (Not fi3)))                       = hi1 == hi2 && fi1 == fi2 && fi1 == fi3                    -- NOT-1
isAxiom (fi1 `Implies` (hi1 `Implies` (fi2 `And` hi2)))                                                     = hi1 == hi2 && fi1 == fi2                                  -- AND-3
isAxiom (fi1 `Implies` (Not fi2) `Implies` _)                                                               = fi1 == fi2                                                -- NOT-2
isAxiom (fi1 `Or` (Not fi2))                                                                                = fi1 == fi2                                                -- NOT-3
isAxiom (fi1 `Implies` (_ `Implies` fi2))                                                                   = fi1 == fi2                                                -- THEN-1 
isAxiom _ = False


modusPonens :: Prop -> Prop -> Prop -> Bool
modusPonens fi1 (fi2 `Implies` psi1) psi2 = fi1 == fi2 && psi1 == psi2
modusPonens _ _ _ = False

proofFrom :: [Prop] -> [Prop] -> Bool
proofFrom gamma proof = all (\fi -> first fi || second fi) proof
    where
        first x  = isAxiom x || elem x gamma
        second x = any (\fip -> elem (fip `Implies` x) prev && modusPonens fip (fip `Implies` x) x) prev  -- modusPonens fip (fip `Implies` x) x may be ommitted
            where 
                prev = takeWhile (/=x) proof

