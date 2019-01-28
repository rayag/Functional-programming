module StructureUtils where
import TextProcessUtils


type Name = String

data Type = StringType | IntType | Complex | Undefined deriving (Show, Eq)

data Structure = 
    SimpleElement Name Type
    | ComplexType Name [Structure] [Structure] deriving Show

structPerson :: Structure
structPerson = (ComplexType "person" [(SimpleElement "name" StringType), (SimpleElement "age" IntType), (ComplexType "address" [(SimpleElement "city" StringType), (ComplexType "street" [(SimpleElement "strname" StringType), (SimpleElement "number" IntType)] [])] [(SimpleElement "country" StringType)])] [(SimpleElement "id" IntType)])

validate :: String -> Type -> Bool
validate val IntType = matches "^[0-9]+$" val
validate _ Complex = False
validate _ _ = True

getElementType :: String -> Structure -> Type
getElementType elementName (SimpleElement name elType) = if elementName == name then elType else Undefined
getElementType elementName (ComplexType name subElements attributes)
    | elementName == name = Complex
    | null elementTypes   = Undefined
    | otherwise           = head elementTypes
        where
            elementTypes = filter (/=Undefined) $ map (getElementType elementName) (subElements ++ attributes)

checkPath :: String -> Structure -> Bool
checkPath path element = case element of 
     (SimpleElement name _)        -> if name == (takeWhile (/='/') first) || path == "" then True else False
     (ComplexType name children _) -> if name == (takeWhile (/='/') first)
                                          then any (checkPath rest) children
                                          else False
    where 
        (_, first, rest) = "^[[:alpha:]]+/?" `matchRegexTriple` path

-- This is for the hardcodded function
checkPathPerson :: String -> Bool
checkPathPerson path = checkPath path structPerson

checkTypeValidityPerson :: String -> String -> Bool
checkTypeValidityPerson propName value = validate value (getElementType prop structPerson)
    where 
        prop = "[[:alpha:]]+$" `matchRegex` propName

getDataTypePerson :: String -> String
getDataTypePerson prop = show $ getElementType ("[[:alpha:]]+$" `matchRegex` prop) structPerson

