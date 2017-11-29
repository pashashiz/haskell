import Control.Monad  
import Data.Char  

-- to compile: ghc --make helloworld

-- 1 hello world
main1 = do  
    putStrLn "Hello, what's your name?"  
    name <- getLine  
    putStrLn ("Hey " ++ name ++ ", you rock!")  

-- 2 with logic
main2 = do
    line <- getLine
    if null line 
        then return () -- return is a constructor monad (in our case IO action), like pure or unit
        else do
            putStrLn (reverseWords line)
            main

reverseWords :: String -> String
reverseWords = unwords . map reverse . words
-- reverseWords s = unwords (map reverse (words s))

-- 3 return example
main3 = do  
    a <- return "hell"  
    b <- return "yeah!"  
    putStrLn $ a ++ " " ++ b 

-- 4 putStr is actually defined recursively with the help of putChar
-- putStr :: String -> IO ()  
-- putStr [] = return ()  
-- putStr (x:xs) = do  
--     putChar x  
--     putStr xs  

-- 5 print is putStrLn . show
main4 = do  print True  
            print 2  
            print "haha"  
            print 3.2  
            print [3,4,3]  

-- 6 when
main5 = do  
    c <- getChar  
    when (c /= ' ') $ do  
        putChar c  
        main  

-- 7 secuence 
main6 = do  
    a <- getLine  
    b <- getLine  
    c <- getLine  
    print [a,b,c]  
-- sequence :: [IO a] -> IO [a]
main7 = do  
    rs <- sequence [getLine, getLine, getLine]  
    print rs  

-- 8 mapM = sequence (map f [a])
-- example: mapM print [1,2,3] will make one IO which will print 1 2 3

-- 9 forever
main8 = forever $ do  
    putStr "Give me some input: "  
    l <- getLine  
    putStrLn $ map toUpper l  

-- 10 forM, it is mapM, only that it has its parameters switched around
main9 = do   
    colors <- forM [1,2,3,4] (\a -> do  
        putStrLn $ "Which color do you associate with the number " ++ show a ++ "?"  
        color <- getLine  
        return color)  
    putStrLn "The colors that you associate with 1, 2, 3 and 4 are: "  
    mapM putStrLn colors  
