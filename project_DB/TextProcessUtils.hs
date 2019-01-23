module TextProcessUtils where
import Data.Char
import Text.Regex.Posix 
import Debug.Trace
import TreeUtils

takeAfter :: String -> String -> String
takeAfter s1 s2 
    | not (isSubarray s1 s2) = trace (s1 ++ "->" ++ s2) error "Element is not present in the code "
    | otherwise = if s1 == sub1 then res else takeAfter s1 (tail s2)
        where 
            sub1 = take (length s1) s2
            res  = drop (length s1) s2

startsWith :: String -> String -> Bool
startsWith str1 str2 = if str1 == beginning then True else False
    where 
        beginning = take len str2
        len = length str1

takeBefore :: String -> String -> String
takeBefore [] _ = []
takeBefore _ [] = []
takeBefore str s@(first:rest) = if startsWith str s then [] else first : (takeBefore str rest)

isSubarray :: Ord a => [a] -> [a] -> Bool
isSubarray xs (y:ys) = begins xs (y:ys) || isSubarray xs ys
    where begins (p:ps) (m:ms) = if p == m then begins ps ms else False
          begins [] _          = True
          begins _ []          = False 
isSubarray _ []     = False     

hasSubElements :: String -> String -> Bool
hasSubElements el code = (length $ filter (=='<') $ middle) > 0
    where 
        middle = dropWhile (/='>') $ takeAfter ("<" ++ el) $ takeBefore ("</" ++ el ++ ">") code

getValue :: String -> String -> String
getValue el code = tail $ dropWhile (/='>') $ takeAfter ("<" ++ el)  $ takeBefore ("</" ++ el ++ ">") code

getData :: String -> [(String,String)]
getData [] = []
getData s  = (element, value) : getData rest
    where element = takeWhile isLetter $ takeWhile (/='>') $ dropWhile (=='<') $ dropWhile (/='<') s
          value      = getValue s element
          rest       = takeAfter  ("</" ++ element ++ ">") s

hasAttributes :: String -> String -> Bool
hasAttributes element s = (length $ filter (=='=') $ elemOpenTagString) > 0
    where 
        elemOpenTagString = takeWhile (/='>') $ takeAfter ("<" ++ element) s

getCurrentElement :: String -> String
getCurrentElement s = takeWhile isValid $ takeWhile (/='>') $ tail $ dropWhile (/='<') s
    where
         isValid c = (c >= 'a' && c <= 'z') || (c >= '0' && c <= '9')

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
                            attributeName  = takeWhile (/='=') $ dropWhile isSpace str
                            attributeValue = takeWhile (/='"') $ tail $ dropWhile (/='"') $ dropWhile (/='=') str 
                            rest           = takeAfter ("\"" ++ attributeValue ++ "\"") str

isList :: String -> String -> Bool
isList _  []  = False
isList [] _   = False
isList el str = (length $ filter (==(head subElements)) subElements) > 1
    where
        subElements = getDirectSubElements el str

takeMiddle :: String -> String -> String
takeMiddle _ [] = []
takeMiddle el s = tail $ takeBefore ("</" ++ el ++ ">") $ dropWhile (/='>') $ takeAfter ("<" ++ el) s

-- Parses xml to create Tree
createTree :: String -> String -> Tree
createTree currentElement s
    | isList currentElement s         = Element currentElement (createListMembers middle) attributeList
    | hasSubElements currentElement s = Element currentElement subTrees attributeList
    | otherwise                       = Element currentElement [(Text val)] attributeList
        where subTrees = map (\x -> createTree x s) $ getDirectSubElements currentElement s
              val      = getValue currentElement s
              attributeList =  getAttributes currentElement openingTag
              openingTag    =  ("<" ++ currentElement) ++ (takeWhile (/='>') $ takeAfter ("<" ++ currentElement) s ++ ">")
              middle   = takeMiddle currentElement s
              createListMembers []  = []
              createListMembers str = first : createListMembers rest
                    where
                        first = createTree child (takeBefore ("</" ++ child ++ ">") str ++ ("</" ++ child ++ ">"))
                        child = getCurrentElement str
                        rest  = dropWhile isSpace $ takeAfter ("</" ++ child ++ ">") str

-- Returns a list of direct subelements of an element according to xml content
getDirectSubElements :: String -> String -> [(String)]
getDirectSubElements el code = if hasSubElements el code then getElems middle else []
    where 
        middle     = dropWhile (/='<') $ dropWhile (/='>') $ takeAfter ("<" ++ el) $ takeBefore ("</" ++ el ++ ">") $ code
        getElems s = 
           if (length s) > 2 then currentElement : getElems rest else []
                 where currentElement = getCurrentElement s
                       rest =  dropWhile (isSpace) $ takeAfter ("</" ++ currentElement ++ ">") s

-- Concats String list using a specified delimiter between elements in the result string
concatListVia :: String -> [String] -> String
concatListVia delimiter = (dropLast len) . (foldr ((++) . (++delimiter)) [])
    where
          len = length delimiter

-- Helper function for concatListVia - drops last n elements from String
dropLast :: Int -> String -> String
dropLast _ [] = []
dropLast n (x:xs)
    | length xs == n = [x]
    | length xs < n  = []
    | otherwise      = x : dropLast n xs

-- Splits String by whiteSpaces
split :: String -> [String]
split [] = []
split str = first : split rest
    where
        first = takeWhile (not . isSpace) $ dropWhile (isSpace) str
        rest  = dropWhile (isSpace) $ takeAfter first str


-- Converts Tree structure to xml-like string
treeToXML :: Tree -> String
treeToXML (Text s)          = s
treeToXML t@(Element _ _ _) = helper 0 t
    where
         helper numTab (Element name subTrees attributes) = tabs ++ openingTag ++ case subTrees of (Text s:[]) -> s ++ closingTag
                                                                                                   _           -> "\n" ++ subElements ++ tabs ++ closingTag 
               where
                    tabs        = replicate numTab '\t'
                    openingTag  = "<" ++ name ++ attrString ++ ">" 
                    closingTag  = "</" ++ name ++ ">"
                    attrString  = space ++ (concatListVia " " $ map (\(attrName, value) -> attrName ++ "=\"" ++ value ++ "\"") attributes)
                    space       = if null attributes then [] else " "
                    subElements = concat $ map (\el -> helper (numTab + 1) el ++ "\n" ) subTrees
         helper _ _ = "" -- this must not be reached

convertToPropList :: String -> [(String,String)]
convertToPropList s 
    | length s > 5 = (name, val) : convertToPropList rest
    | otherwise    = []
    where 
        name = takeWhile (not . isSpace) $ takeWhile (/='=') $ dropWhile isSpace s
        val  = takeWhile (/='\"') $ tail $ dropWhile (/='\"') $ dropWhile (/='=') s
        rest = dropWhile isSpace $ dropWhile (=='\"') $ takeAfter val s

toInt :: String -> Int
toInt s = foldl (\h t -> h * 10 + (toDig t)) 0 s
    where toDig c = fromIntegral $ ord c - ord '0'


matches :: String -> String -> Bool
matches reg str = str =~ reg

matchRegex :: String -> String -> String
matchRegex reg str = str =~ reg
        
