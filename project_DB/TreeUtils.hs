module TreeUtils where

type Attribute = (String, String)

data Tree = Element String [Tree] [Attribute] | Text String  deriving (Show, Eq)

-- Adds a subtree on the relevant place
addTree :: Tree -> Tree -> Tree
addTree tree@(Element _ [Text _] _) _                              = tree  -- if this is reached than there is no suitable place for the new tree to be added
addTree (Element parName subTrees attrList) new@(Element name _ _) = if isSet && childMatches 
                                                                     then (Element parName (subTrees ++ [new]) attrList)
                                                                     else (Element parName (map (\subTree -> addTree subTree new) subTrees) attrList)
    where 
        subNames                         = map (\(Element name _ _) -> name) subTrees
        isSet                            = all (==(head subNames)) subNames
        childMatches                     = (head subNames) == name



-- Returns the value of a specified property within a tree
-- if path evaluates to a composite value, then the whole subtree is displayed
getProperty :: String -> Tree -> [String]
getProperty prop (Element elName (Text elVal:[]) _) = if elName == prop then [elVal] else [] -- if names match, then property value should be returned
getProperty prop (Element elName children attributes) 
    | elem '/' prop = if elName == name 
                       then concat $ map (getProperty rest) children -- the rest of the address should be looked for within the children
                       else concat $ map (getProperty prop) children -- the whole address is somewhere within current element subtrees
    | otherwise = if name == elName then [treeToString t] else []    -- the only chances are that this is the subtree we are looking for
    where 
        name = takeWhile (/='/') prop
        rest = dropWhile (=='/') $ dropWhile (/='/') prop

-- For elements that act like lists (a.k.a their subelements are of the same type)
-- returns the value of the subelement on specified index
getPropAtIndex :: String -> Int -> Tree -> String
getPropAtIndex propName index (Element _ subTrees _)
    | index < 0 || index > length subTrees = "Invalid index"
    | otherwise                            = concat $ getProperty propName (subTrees !! index) 
getPropAtIndex _ _ _ = []

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
changeProperty propName newVal (Element elName children attributes)       = (Element elName newChildren attributes)
    where
        newChildren = map (changeProperty propName newVal) children

-- Returns subtree named in a specific way
-- if an element is a set of elements, then it returns the first of them
getSubTree :: String -> Tree -> Tree
getSubTree elName tree@(Element name [Text _] attrList) = if elName == name then tree else Text "Empty"
getSubTree elName tree@(Element name subTrees attrList)
    | name == elName = tree
    | isSet tree     = getSubTree elName (head subTrees)
    | otherwise      = helper elName subTrees
        where
            helper _ []                                   = Text "Empty"
            helper elName (first@(Element name _ _):rest) = if  firstSubTree /= Text "Empty" then firstSubTree else helper elName rest
                where 
                    firstSubTree = getSubTree elName first

makeDefault :: Tree -> Tree
makeDefault (Element name [Text _] attrList) = (Element name [Text ""] defAttrList)
    where defAttrList = map (\(name, _) -> (name, "")) attrList
makeDefault (Element name subTrees attrList) = (Element name defSubTrees defAttrList)
    where
        defSubTrees = map makeDefault subTrees
        defAttrList = map (\(name, _) -> (name, "")) attrList

modifyFromList :: [(String, String)] -> Tree -> Tree
modifyFromList lst t = foldl (\nv (elName, value) -> changeProperty elName value nv) t lst

isSet :: Tree -> Bool
isSet (Element name subTrees _) = all (==(head subNames)) subNames
    where
        subNames = map (\(Element name _ _) -> name) subTrees
isSet _                         = False

-- Returns list of subtrees which have properties specified by a condition
-- For example: subTreesByProperty "age" "21" t will return a list of subtrees whose elements with names "age" evaluate to "23"
subTreesByProperty :: String -> String -> Tree -> [Tree]
subTreesByProperty prop val tree@(Element name subTrees attrList)
    | isSet t = concat $ filter (not . null) $ map (subTreesByProperty prop val) subTrees -- TODO 
    | otherwise = if hasProp tree then [tree] else []
        where 
            hasProp tr = (concat $ getProperty prop tr)== val -- depends on getProperty!!!

changePropByCondition :: String -> String -> String -> String -> Tree -> Tree
changePropByCondition condProp condValue prop value tree@(Element name subTrees attrValue)
    | isSet tree   = Element name (map changeByCondSubtrees subTrees) attrValue
    | otherwise = changeByCondSubtrees tree
        where 
            changeByCondSubtrees tree = if concat (getProperty condProp tree) == condValue then changeProperty prop value tree else tree

deleteSubTreeByCondition :: String -> String -> Tree -> Tree
deleteSubTreeByCondition _ _ tree@(Text _) = tree
deleteSubTreeByCondition prop val tree@(Element name subTrees attrList)
    | isSet tree = Element name filteredSubTrees attrList
    | otherwise  = Element name (map (deleteSubTreeByCondition prop val) subTrees) attrList
    where
        filteredSubTrees = filter (\subtree -> (concat $ getProperty prop subtree) /= val) subTrees

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


t :: Tree
t = (Element "people" [Element "person" [Element "name" [Text "Mimi"] [],Element "age" [Text "15"] [],Element "address" [Element "city" [Text "Sofia"] [],Element "street" [Element "strname" [Text "Rakovska"] [],Element "number" [Element "prop1" [Text "p1_value"] [],Element "prop2" [Text "p2_value"] []] []] []] []] [("id","1")],Element "person" [Element "name" [Text "Mimi"] [],Element "age" [Text "23"] [],Element "address" [Element "city" [Text "Pernik"] [],Element "street" [Element "strname" [Text "Golf"] [],Element "number" [Element "prop1" [Text "gosho_value"] [],Element "prop2" [Text "p2_value"] []] []] []] []] [("id","2")],Element "person" [Element "name" [Text "Pesho"] [],Element "age" [Text "30"] [],Element "address" [Element "city" [Text "Veliko Tarnovo"] [],Element "street" [Element "strname" [Text "Street_Pesho"] [],Element "number" [Element "prop1" [Text "pesho_value"] [],Element "prop2" [Text "p2_value"] []] []] []] []] [("id","3")]] [])




