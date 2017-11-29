import System.IO
import System.Environment  
import Data.List  
import Data.Char

main1 = do
    handle <- openFile "girlfriends.txt" ReadMode
    contents <- hGetContents handle
    putStr contents
    hClose handle

-- using withFile
main2 = do     
    withFile "girlfriends.txt" ReadMode (\handle -> do  
        contents <- hGetContents handle     
        putStr contents)  

withFile' :: FilePath -> IOMode -> (Handle -> IO a) -> IO a  
withFile' path mode f = do  
    handle <- openFile path mode   
    result <- f handle  
    hClose handle  
    return result  

-- reading file
main3 = do  
    contents <- readFile "girlfriends.txt"  
    putStr contents  

--writing file (just rewtires a file, there are appendFile, ...)
main4 = do     
    contents <- readFile "girlfriends.txt"     
    writeFile "girlfriendscaps.txt" (map toUpper contents)  

-- with buffering
main5 = do   
    withFile "something.txt" ReadMode (\handle -> do  
        hSetBuffering handle $ BlockBuffering (Just 2048) -- makes a new buffered IO action
        contents <- hGetContents handle  
        putStr contents)  

main = do  
   args <- getArgs  
   progName <- getProgName  
   putStrLn "The arguments are:"
   mapM putStrLn args  
   putStrLn "The program name is:"  
   putStrLn progName 