import System.IO
import Data.Char
import Debug.Trace
import TextProcessUtils
import TreeUtils

processAdd :: String -> Tree -> IO()
processAdd args t = writeFile "file2.txt" $ treeToXML $ addTree t $ modifyFromList propList $ makeDefault $ getSubTree name t
    where 
        name = takeWhile (not . isSpace) args
        propList = convertToPropList $ takeAfter name args

processDelete :: String -> Tree -> IO()
processDelete args t 
    | length argLst == 2 = do
                             writeFile "file2.txt" $ treeToXML $ deleteSubTreeByCondition prop condVal t
                             putStrLn "Delete executed successfully"
    | otherwise          = putStrLn "Wrong number of arguments! Usage: delete <cond_prop> <cond_prop_val>"
        where
            argLst  = split args
            prop    = argLst !! 0
            condVal = argLst !! 1

processUpdate :: String -> Tree -> IO()
processUpdate args t 
    | length argLst == 4 = do
                              writeFile "file2.txt" $ treeToXML $ changePropByCondition prop cond elem newVal t
                              putStrLn "Update executed successfully"
    | otherwise = putStrLn $ "Wrong number of arguments! Usage update <cond_prop> <cond_prop_val> <new_prop> <new_prop_val>"
    where
        argLst = split args 
        prop   = argLst !! 0
        cond   = argLst !! 1
        elem   = argLst !! 2
        newVal = argLst !! 3

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
            isCond        = elem '=' args

main = do 
    contents <- readFile "file1.txt"
    input <- getLine
    let command = takeWhile (not . isSpace) input
        args = dropWhile (isSpace) $ takeAfter command input
    
    case command of "add"    -> processAdd args (createTree "people" contents)
                    "update" -> processUpdate args (createTree "people" contents)
                    "delete" -> processDelete args (createTree "people" contents)
                    "read"   -> processRead args (createTree "people" contents)
                    xs       -> putStrLn "Invalid command"
    hFlush stdout
    main 
    --processCommand command args
    --writeFile "file2.txt" new
