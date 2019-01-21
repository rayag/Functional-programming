import System.IO
import Data.Char
import Debug.Trace
import TextProcessUtils
import TreeUtils
import StructureUtils

main = do 
    contents <- readFile "file1.txt"
    let t = (Element "people" [(Element "person" [Element "name" [Text "Mimi"] [], Element "age" [Text "23"] []] [])] [])
        t1 = (Element "psrson" [Element "name" [Text "Pesho"] [], Element "age" [Text "15"] []] [])
    command <- getLine
    putStr $ treeToString $ addTree t t1
    writeFile "file2.txt" contents
