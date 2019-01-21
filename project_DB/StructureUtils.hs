module StructureUtils where
import TreeUtils

data Structure = SElement String [Structure] [String] | Empty deriving (Show)

-- Creates a structure according to the structure of an existing tree
createStructureFromTree :: Tree -> Structure
createStructureFromTree (Element name [Text _] attrList) = SElement name [] (map fst attrList)
createStructureFromTree (Element name subTrees attrList)
    | isSet = SElement name [(createStructureFromTree (head subTrees))] structAttrList
    | otherwise = SElement name subStructures structAttrList
        where 
            subNames = map (\ e@(Element name _ _) -> name) subTrees
            isSet = all (==(head subNames)) subNames
            structAttrList = map fst attrList
            subStructures = map createStructureFromTree subTrees


