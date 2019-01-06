import System.IO
import Debug.Trace

takeAfter :: String -> String -> String
takeAfter s1 s2 
    | not (isSubarray s1 s2) = trace s1 error "Element is not present in the code "
    | otherwise = if s1 == sub1 then res else takeAfter s1 (tail s2)
        where 
            sub1 = take (length s1) s2
            res  = drop (length s1) s2

takeBefore :: String -> String -> String
takeBefore s1 s2 = if s1 == sub then res else takeBefore s1 (takeExcept 1 s2)
    where 
        sub = takeLast (length s1) s2
            where
                  takeLast n (x:xs) = if n == (length xs) then xs else takeLast n xs
                  takeLast n []     = []
        res = takeExcept (length s1) s2
        takeExcept n (x:xs) = if n == (length xs) then [x] else [x] ++ takeExcept n xs
        takeExcept n []     = []

isSubarray :: Ord a => [a] -> [a] -> Bool
isSubarray xs (y:ys) = begins xs (y:ys) || isSubarray xs ys
    where begins (p:ps) (m:ms) = if p == m then begins ps ms else False
          begins [] _          = True
          begins _ []          = False 
isSubarray xs []     = False     

hasSubElements :: String -> String -> Bool
hasSubElements el code = (length $ filter (=='<') $ middle) > 0
    where middle = dropWhile (/='>') $ takeAfter ("<" ++ el) $ takeBefore ("</" ++ el ++ ">") code

getValue :: String -> String -> String
getValue el code = tail $ dropWhile (/='>') $ takeAfter ("<" ++ el)  $ takeBefore ("</" ++ el ++ ">") code

getData :: String -> [(String,String)]
getData [] = []
getData s = (element, value) : getData rest
    where element = takeWhile isLetter $ takeWhile (/='>') $ dropWhile (=='<') $ dropWhile (/='<') s
          isLetter c = c >= 'a' && c <= 'z'
          value   = getValue s element
          rest    = takeAfter  ("</" ++ element ++ ">") s

tupleToStr :: (String, String) ->  String
tupleToStr (el, val) = "(" ++ el ++ ", " ++ val ++ ")"

boolToString :: Bool -> String
boolToString b 
    | b = "True"
    | otherwise  = "False"

hasAttributes :: String -> String -> Bool
hasAttributes element s = (length $ filter (=='=') $ elemOpenTagString) > 0
    where 
        elemOpenTagString = takeWhile (/='>') $ takeAfter ("<" ++ element) s

type Attribute = (String, String)

data Tree = Element String [Tree] [Attribute] | Text String  deriving (Show)

s :: String
s = "<person> <name>Mimi</name> <age>15</age> </person>"
s1 = "<person> Person1 </person>"

getCurrentElement :: String -> String
getCurrentElement s = takeWhile isLetter $ takeWhile (/='>') $ tail $ dropWhile (/='<') s
    where
         isLetter c = c >= 'a' && c <= 'z'

-- Returns a list containing all the attributes for current element
getAttributes :: String -> String -> [(Attribute)]
getAttributes currentElement s 
    | hasAttributes currentElement s = getAttrList attributes
    | otherwise       = []
        where
             attributes  = takeAfter ("<" ++ currentElement) $ s
             getAttrList str
                 | elem '=' str = (attributeName, attributeValue) : getAttrList rest
                 | otherwise    = []
                        where 
                            attributeName  = takeWhile (/='=') $ dropWhile (==' ') str
                            attributeValue = takeWhile (/='"') $ tail $ dropWhile (/='"') $ dropWhile (/='=') str 
                            rest           = takeAfter ("\"" ++ attributeValue ++ "\"") str

createTree :: String -> String -> Tree
createTree currentElement s
    | hasSubElements currentElement s = Element currentElement subTrees attributeList
    | otherwise                       = Element currentElement [(Text val)] attributeList
        where subTrees = map (\x -> createTree x s) $ getDirectSubElements currentElement s
              val      = getValue currentElement s
              attributeList = getAttributes currentElement s

getDirectSubElements :: String -> String -> [(String)]
getDirectSubElements elem code = if hasSubElements elem code then getElems middle else []
    where 
        middle = dropWhile (/='>') $ takeAfter ("<" ++ elem) $ takeBefore ("</" ++ elem ++ ">") $ code
        getElems s = 
           if (length s) > 2 then currentElement : getElems rest else []
                 where currentElement = getCurrentElement s
                       rest = filter (/=' ') $ takeAfter ("</" ++ currentElement ++ ">") s

concatList :: [String] -> String
concatList = foldr (++) []

treeToString :: Tree -> String
treeToString (Text s) = s
treeToString (Element name subTrees attributes) = "<" ++ name ++ " " ++ attrString ++ ">\n" ++ subElements ++ "</" ++ name ++ ">\n" 
    where
         attrString = concatList $ map (\attr@(name, value) -> name ++ "=\"" ++ value ++ "\" ") attributes
         subElements = concatList $ map (\el -> "\t" ++ treeToString el ++ "\n") subTrees
         

main = do 
    contents <- readFile "file1.txt"
    putStr $ treeToString $ createTree "person" s1

    
