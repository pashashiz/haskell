import System.Environment  
import qualified Data.ByteString.Lazy as B  
import qualified Data.ByteString as S 

-- pack
-- pack bytes (Word8s) into ByteString which is a list of chanks Chunk "s1" (Chunk "s2" Empty)
-- pack :: [Word8] -> ByteString
-- B.pack [99,97,110] --> Chunk "can" Empty 

-- cons
-- lazy
-- B.cons 85 $ B.pack [80,81,82,84] --> Chunk "U" (Chunk "PQRT" Empty)  
-- strict
-- foldr B.cons' B.empty [50..60] --> Chunk "23456789:;<" Empty 

main = do  
    (fileName1:fileName2:_) <- getArgs  
    copyFile fileName1 fileName2  
  
copyFile :: FilePath -> FilePath -> IO ()  
copyFile source dest = do  
    contents <- B.readFile source  
    B.writeFile dest contents  