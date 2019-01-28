import System.IO
import Control.Exception
import Data.Char
import Debug.Trace
import TextProcessUtils
import TreeUtils
import StructureUtils

-- Commands are of type: add <oject_type> [<element_name>=<element_value>]*
processAdd :: String -> Tree -> IO()
processAdd args t 
    | not validPaths = putStrLn "Specified path to property is not valid. It is misspelled or the path to it is wrong."
    | length invalidDataTypes > 0 = putStrLn $ "Data entered is not of relevant type. Problems: " ++ problems
    | otherwise  = writeFile "file1.txt" $ treeToXML $ fixIds $ addTree t $ modifyFromList propList $ makeDefault $ head $ getSubTrees name t
    where 
        name = takeWhile (not . isSpace) args
        propList = convertToPropList name $ takeAfter name args
        validPaths = all (\(prop, _) -> checkPathPerson prop) propList
        invalidDataTypes = filter (\(prop, value) -> not $ checkTypeValidityPerson prop value) propList
        problems = concat $ map (\(prop, _) -> prop ++ " -> " ++ (getDataTypePerson prop) ++ "\n") invalidDataTypes

processDelete :: String -> Tree -> IO()
processDelete args t 
    | length argLst == 2 && checkPathPerson prop = do
                                                     writeFile "file1.txt" $ treeToXML $ fixIds $ deleteSubTreeByCondition prop condVal t
                                                     putStrLn "Delete executed successfully"
    | otherwise          = putStrLn "Wrong number of arguments! Usage: delete <cond_prop> <cond_prop_val>"
        where
            argLst  = split args
            prop    = argLst !! 0
            condVal = argLst !! 1

processUpdate :: String -> Tree -> IO()
processUpdate args t 
    | not $ checkTypeValidityPerson el newVal = putStrLn $ "Invalid data type" ++ getDataTypePerson el
    | not $ checkPathPerson prop              = putStrLn $ "Wrong property path: " ++ prop
    | otherwise = do
                              writeFile "file1.txt" $ treeToXML $ changePropByCondition prop cond el newVal t
                              putStrLn "Update executed successfully"
    | otherwise = putStrLn $ "Wrong number of arguments! Usage update <cond_prop> <cond_prop_val> <new_prop> <new_prop_val>"
    where
        argLst      = matchGroups "([[:alpha:]/]+)[[:space:]]+\"([[:alpha:][:space:]]+)\"" args
        objName     = matchRegex "^[[:alpha:]]+" args
        prop        = fixPath $ argLst !! 0 !! 1
        cond        = argLst !! 0 !! 2
        el          = fixPath $ argLst !! 1 !! 1
        newVal      = argLst !! 1 !! 2
        fixPath arg = if ("^" ++ objName ++ "/") `matches` arg then arg else objName ++ "/" ++ arg


processRead :: String -> Tree -> IO()
processRead args t
    | isIndex       = putStrLn $ show $ getPropAtIndex prop index t
    | isPath        = putStrLn $ concatListVia "\n" $ getProperty args t
    | isAttrListing = putStrLn $ show $ getAttribute attr t
    | otherwise     = trace prop $ putStrLn "otherwise"
        where 
            isIndex       = matches "[a-zA-Z]+\\[[0-9]+\\]" args
            index         = toInt $ matchRegex "[0-9]+" $ matchRegex "\\[[0-9]+\\]" args
            isPath        = matches "^[[:alpha:]]+(/[[:alpha:]]+)*$" args
            isAttrListing = matches "[a-z]+\\(@[a-zA-Z0-9_]+\\)" args
            attr          = matchRegex "[^@]+" $ matchRegex "@[a-zA-Z0-9_]+" args
            prop          = matchRegex "^[^[]+" args


main :: IO()
main = do 
    inFile <- openFile "file1.txt" ReadMode
    contents <- hGetContents inFile
    evaluate $ length contents
    hClose inFile
    input <- getLine
    let command = takeWhile (not . isSpace) input
        args = dropWhile (isSpace) $ takeAfter command input
    
    case command of "add"    -> processAdd args (createTree "people" contents)
                    "update" -> processUpdate args (createTree "people" contents)
                    "delete" -> processDelete args (createTree "people" contents)
                    "read"   -> processRead args (createTree "people" contents)
                    _        -> putStrLn "Invalid command"
    hFlush stdout
    main 
