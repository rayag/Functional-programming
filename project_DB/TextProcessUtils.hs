module TextProcessUtils where
import Data.Char
import Text.Regex.Posix 
import TreeUtils


-- Returns the substring following another substring
-- Example: takeAfter "love" "I love my mum" -> " my mum"
takeAfter :: String -> String -> String
takeAfter s1 s2 
    | not (isSubarray s1 s2) = error "Element is not present in the code "
    | otherwise = if s1 == sub1 then res else takeAfter s1 (tail s2)
        where 
            sub1 = take (length s1) s2
            res  = drop (length s1) s2

-- Returns True if str2 starts with str 1
-- Example: startsWith "Cats" "Cats are beautiful" -> True
--          startsWith "Cats" "I love cats"        -> False
startsWith :: String -> String -> Bool
startsWith str1 str2 = if str1 == beginning then True else False
    where 
        beginning = take len str2
        len = length str1

-- Returns the substring before a specified substring not including it
-- Example: takeBefore "cats" "I love cats" -> "I love "
takeBefore :: String -> String -> String
takeBefore [] _ = []
takeBefore _ [] = []
takeBefore str s@(first:rest) = if startsWith str s then [] else first : (takeBefore str rest)

-- Returns True if an array is a subarray of an other array
-- Example: isSubArray "cats" "I love cats" -> True
--          isSubarray "cats" "Dogs make me happy" -> False
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

-- For simple elements returns the value of the element
-- Example: getValue "radius" "<radius>10</radius>" -> "10"
getValue :: String -> String -> String
getValue el code = tail $ dropWhile (/='>') $ takeAfter ("<" ++ el)  $ takeBefore ("</" ++ el ++ ">") code

-- Returns True if element has attributes in the xml file
hasAttributes :: String -> String -> Bool
hasAttributes element s = (length $ filter (=='=') $ elemOpenTagString) > 0
    where 
        elemOpenTagString = takeWhile (/='>') $ takeAfter ("<" ++ element) s

-- Returns first element from the string
-- Example: getCurrentElement "<cicle>....</circle>" -> "circle"
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

-- Returns True when subelements in the XML file have equal names
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

-- Converts a string to a list suitable for use in TreeUtils functions
-- Generated list is of type [(<element_name>, <element_value>), (<attribute_name>, <attribute_value>)]
-- For TreeUtils functions paths should be for example "person/name" and
-- not just "name" 
-- Example: convertToPropList "person" "name=\"Hristo\" person/age        =    \"24\"" -> [("person/name", "Hristo"), ("person/age", "24")]
convertToPropList :: String -> String -> [(String,String)]
convertToPropList objType args 
    | length args > 5 = (path, val) : convertToPropList objType rest
    | otherwise    = []
    where 
        first = takeWhile (not . isSpace) $ takeWhile (/='=') $ dropWhile isSpace args
        path = if matches ("^" ++ objType ++ "/.*") first then first else objType ++ "/" ++ first
        val  = takeWhile (/='\"') $ tail $ dropWhile (/='\"') $ dropWhile (/='=') args
        rest = dropWhile isSpace $ dropWhile (=='\"') $ takeAfter val args

-- Converts a sting to Int
-- Example: "123" -> 123
-- Caution: It does not have built-in check for input.
toInt :: String -> Int
toInt s = foldl (\h t -> h * 10 + (toDig t)) 0 s
    where toDig c = fromIntegral $ ord c - ord '0'

-- Returns True if passed regex matches the passed string
-- Example: matches "^[0-9]+$" "123" -> True
--          matches "^[0-9]+$" "12A" -> False
matches :: String -> String -> Bool
matches reg str = str =~ reg

-- Returns the part of the string that matches the regex and "" if there is no such part
-- Example: matchRegex "^[0-9]+" "1234K" -> "1234"
--          matchRegec "^[0-9]+$" "1234K" -> ""
matchRegex :: String -> String -> String
matchRegex reg str = str =~ reg

-- Returns triple containing (Before match string, Matched string, Remaining part of the string)
-- Example: matchRegexTriple "[0-9]+" "vasko123kote" -> ("vasko", "123", "kote")
matchRegexTriple :: String -> String -> (String, String, String)
matchRegexTriple reg str = str =~ reg

matchGroups :: String -> String -> [[String]]
matchGroups reg str = str =~ reg





    
