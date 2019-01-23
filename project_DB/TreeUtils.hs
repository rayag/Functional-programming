module TreeUtils where
--import Debug.Trace

type Attribute = (String, String)

data Tree = Element String [Tree] [Attribute] | Text String  deriving (Show, Eq)

-- Adds a subtree on the relevant place
addTree :: Tree -> Tree -> Tree
addTree tree@(Element _ [Text _] _) _                                   = tree  -- if this is reached than there is no suitable place for the new tree to be added
addTree tree@(Element parName subTrees attrList) new@(Element name _ _) = if isSet tree && childMatches 
                                                                           then (Element parName (subTrees ++ [new]) attrList)
                                                                           else (Element parName (map (\subTree -> addTree subTree new) subTrees) attrList)
    where 
        subNames     = map (\(Element subTreeName _ _) -> subTreeName) subTrees
        childMatches = (head subNames) == name
addTree tree (Text _)                                                   = tree
addTree tree _                                                          = tree



-- Returns the value of a specified property within a tree
-- if path evaluates to a composite value, then the whole subtree is displayed
getProperty :: String -> Tree -> [String]
getProperty prop (Element elName (Text elVal:[]) _) = if elName == prop then [elVal] else [] -- if names match, then property value should be returned
getProperty prop tree@(Element elName children _)  
    | elName == name && (elem '/' prop) = concat $ map (getProperty rest) children -- the rest of the address should be looked for within the children
    | elName == name                  = [treeToString tree]
    | otherwise                       = []                  
    where 
        name = takeWhile (/='/') prop
        rest = dropWhile (=='/') $ dropWhile (/='/') prop
getProperty _ _ = [] -- This case must not be reached

-- For elements that act like lists (a.k.a their subelements are of the same type)
-- returns the value of the subelement on specified index
getPropAtIndex :: String -> Int -> Tree -> String
getPropAtIndex address index tree = helper subtree
    where 
        subtree = getSubTreeAtIndex propName index tree
        helper Nothing   = ""
        helper (Just tr) = concat $ getProperty address tr
        propName = takeWhile (/='/') address
            
-- Returns the values of this attribute among all subTrees within the specified (if it exists)
getAttribute :: String -> Tree -> [String]
getAttribute attrName (Element _ subTrees attrList) 
    | length attrValues == 0 = concat $ map (getAttribute attrName) subTrees
    | otherwise              = attrValues
        where
             attrValues = map snd $ filter (\el -> fst el == attrName) attrList
getAttribute _ _             = []

-- Changes the value of a specified property within the Tree
-- For Examlple: changeProperty "name" "Pesho" (Element "person" [(Element "name" [Text "Gosho"] [])] []) -> (Element "person" [(Element "name" [Text "Pesho"] [])] [])
changeProperty :: String -> String -> Tree -> Tree
changeProperty propName newVal el@(Element elName (Text _:[]) attributes) = if propName == elName then (Element elName (Text newVal:[]) attributes) else el
changeProperty address newVal el@(Element elName children attributes)     = if first == elName then (Element elName newChildren attributes) else el
    where
        newChildren = map (changeProperty rest newVal) children
        first = takeWhile (/='/') address
        rest  = dropWhile (=='/') $ dropWhile (/='/') address
changeProperty _ _ el@(Text _)                                            = el -- This case must not be reached

changeAttribute :: String -> String -> Tree -> Tree
changeAttribute requestedName newVal (Element name subTrees attrList) = (Element name newSubTrees newAttrList)
    where
        newAttrList = foldr (\(attrName, val) rest -> if attrName == requestedName then (attrName, newVal) : rest else (attrName, val) : rest) [] attrList
        newSubTrees = map (changeAttribute requestedName newVal) subTrees
changeAttribute _ _ el@(Text _) = el

getSubTreeAtIndex :: String -> Int -> Tree -> Maybe Tree
getSubTreeAtIndex elName index tree@(Element _ subTrees _) 
    | index < 0 || index >= length subTrees = Nothing
    | otherwise = Just ((getSubTrees elName tree) !! index)
getSubTreeAtIndex _ _ (Text _) = Nothing

-- Returns subtree named in a specific way
-- if an element is a set of elements, then it returns the first of them
getSubTrees :: String -> Tree -> [Tree]
getSubTrees elName tree@(Element name [Text _] _) = if elName == name then [tree] else []
getSubTrees elName tree@(Element name subTrees _)
    | name == elName = [tree]
    | otherwise      = concat $ filter (not . null) $ map (getSubTrees elName) subTrees
getSubTrees _ (Text _)                            = [] -- Text elements do not have subtrees

-- Creates a tree which contains default properties. This is especially needed when
-- a new tree is added, as its structure is directly gotten from the tree. This means
-- that when a new tree is created it must follow the already established structure
-- (have the same elements, subelements, attributes)
makeDefault :: Tree -> Tree
makeDefault (Element name [Text _] attrList) = (Element name [Text ""] defAttrList)
    where defAttrList = map (\(attrName, _) -> (attrName, "")) attrList
makeDefault (Element name subTrees attrList) = (Element name defSubTrees defAttrList)
    where
        defSubTrees = map makeDefault subTrees
        defAttrList = map (\(attrName, _) -> (attrName, "")) attrList
makeDefault (Text _)                         = Text ""

-- Returns True if all the subtrees of the current element are of the same type (in the sense that they have the same names)
-- Text Elements could not have that property
isSet :: Tree -> Bool
isSet (Element _ [Text _] _) = False
isSet (Element _ subTrees _) = all (==(head subNames)) subNames
    where
        subNames = map (\(Element name _ _) -> name) subTrees
isSet _                      = False

-- Returns True, if the tree has a property is equal to the requested one
-- Example: "name" "Ivan" (Element "person" [(Element "name" [Text "Ivan"] [])] []) -> True
--          "name" "Mimi" (Element "person" [(Element "name" [Text "Ivan"] [])] []) -> False
satisfiesCondition :: String -> String -> Tree -> Bool
satisfiesCondition prop value tree                        = any (==value) $ getProperty prop tree -- if any of the subtrees satisfies the condition, then this one does

changePropByCondition :: String -> String -> String -> String -> Tree -> Tree
changePropByCondition condProp condValue prop value tree@(Element name subTrees attrValue)
    | isSet tree   = Element name (map changeByCondSubtrees subTrees) attrValue
    | otherwise    = changeByCondSubtrees tree
        where 
            changeByCondSubtrees subtree = if satisfiesCondition condProp condValue subtree
                                            then changeProperty prop value subtree 
                                            else subtree
changePropByCondition _ _ _ _ tree@(Text _) = tree --program must not reach this condition, but if this happens, then nothing will be done

deleteSubTreeByCondition :: String -> String -> Tree -> Tree
deleteSubTreeByCondition _ _ tree@(Text _) = tree
deleteSubTreeByCondition prop val tree@(Element name subTrees attrList)
    | isSet tree = Element name filteredSubTrees attrList
    | otherwise  = Element name (map (deleteSubTreeByCondition prop val) subTrees) attrList
    where
        filteredSubTrees = filter (not . (satisfiesCondition prop val)) subTrees

-- If specified element is a list then returns length of that list
-- Else returns 0
lengthTree :: String -> Tree -> Int
lengthTree name tree@(Element elName subTrees _)
    | name == elName && isSet tree = length subTrees
    | name == elName               = 0
    | otherwise                    = maximum (map (lengthTree name) subTrees)
lengthTree _ _                     = 0

treeToString :: Tree -> String
treeToString tree = helper 0 tree
    where
        helper numTab (Element name [Text value] _) = replicate numTab '\t' ++ name ++ ": " ++ value ++ "\n"
        helper numTab (Element name subTrees _    ) = replicate numTab '\t' ++ name ++ ":\n" ++ subTreesString
            where 
                subTreesString = concat $ map (helper (numTab + 1)) subTrees
        helper _ (Text _)                           = "" -- this should under no means happen

modifyFromList :: [(String, String)] -> Tree -> Tree
modifyFromList [] t  = t
modifyFromList lst t = foldl (\nv (elName, value) -> changeProperty elName value nv) t lst



t1 :: Tree
t1 = (Element "people" [Element "person" [Element "name" [Text "Mimi"] [],Element "age" [Text "15"] [],Element "address" [Element "city" [Text "Sofia"] [],Element "street" [Element "strname" [Text "Rakovska"] [],Element "number" [Element "digit" [Text "1"] [],Element "digit" [Text "2"] []] []] []] []] [("id","1")],Element "person" [Element "name" [Text "Gosho"] [],Element "age" [Text "23"] [],Element "address" [Element "city" [Text "Pernik"] [],Element "street" [Element "strname" [Text "Golf"] [],Element "number" [Element "digit" [Text "3"] [],Element "digit" [Text "3"] []] []] []] []] [("id","2")],Element "person" [Element "name" [Text "Pesho"] [],Element "age" [Text "30"] [],Element "address" [Element "city" [Text "Veliko Tarnovo"] [],Element "street" [Element "strname" [Text "Street_Pesho"] [],Element "number" [Element "digit" [Text "4"] [],Element "digit" [Text "8"] []] []] []] []] [("id","3")]] [])




