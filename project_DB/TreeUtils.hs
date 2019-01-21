module TreeUtils where

type Attribute = (String, String)

data Tree = Element String [Tree] [Attribute] | Text String  deriving (Show)

-- Adds a subtree on the relevant place
addTree :: Tree -> Tree -> Tree
addTree tree@(Element _ [Text _] _) _                                   = tree
addTree tree@(Element parName subTrees attrList) new@(Element name _ _) = if isSet && childMatches 
                                                                          then (Element parName (subTrees ++ [new]) attrList)
                                                                          else (Element parName (tryChildren subTrees new) attrList)
    where 
        subNames                         = map (\(Element name _ _) -> name) subTrees
        isSet                            = all (==(head subNames)) subNames
        childMatches                     = (head subNames) == name
        tryChildren [] _                 = []
        tryChildren (first:rest) newTree = addTree first newTree : tryChildren rest newTree


-- Returns the value of a specified property within a tree
getProperty :: String -> Tree -> [String]
getProperty prop (Element elName (Text elVal:[]) _) = if elName == prop then [elVal] else []
getProperty [] _                                    = []
getProperty prop (Element elName children attributes)    = if elName == name 
                                                           then concat $ map (getProperty rest) children
                                                           else concat $ map (getProperty prop) children 
    where 
        name = takeWhile (/='/') prop
        rest = tail $ dropWhile (/='/') prop

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



