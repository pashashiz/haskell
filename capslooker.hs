import Data.Char  
  
main = do  
    contents <- getContents  
    putStr (map toUpper contents)  

-- short lines - 1
main2 = do  
    contents <- getContents  
    putStr (shortLinesOnly contents)  

-- short lines - 2
main3 = interact shortLinesOnly 
  
shortLinesOnly :: String -> String  
shortLinesOnly input =   
    let allLines = lines input  
        shortLines = filter (\line -> length line < 10) allLines  
        result = unlines shortLines  
    in  result  