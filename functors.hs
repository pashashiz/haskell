-- 1. functors

-- class Functor f where
--     fmap :: (a -> b) -> f a -> f b

-- instance Functor Maybe where  
--     fmap f (Just x) = Just (f x)  
--     fmap f Nothing = Nothing  

-- fmap (replicate 3) [1,2,3,4] -> [[1,1,1],[2,2,2],[3,3,3],[4,4,4]]  

-- 2. applicative functors
-- fmap (*) (Just 3)
-- what if we want take out a function from Just (3 *) and map it over Just 5

-- class (Functor f) => Applicative f where  
--     pure :: a -> f a  
--     (<*>) :: f (a -> b) -> f a -> f b  

-- instance Applicative Maybe where  
--     pure = Just  
--     Nothing <*> _ = Nothing  
--     (Just f) <*> something = fmap f something 

-- Just (+3) <*> Just 9  -> Just 12
-- pure (+3) <*> Just 9  -> Just 12
-- Just (++"hahah") <*> Nothing -> Nothing
-- Nothing <*> Just "woot" -> Nothing

-- multiple applications pipeline:
-- pure (+) <*> Just 3 <*> Just 5  
-- (pure (+) <*> Just 3) <*> Just 5
-- Just (+3) <*> Just 5
-- Just 8

-- infix fmap
-- f <$> x = pure f <*> x = fmap f x
-- now we can write
-- f <$> x <*> y <*> z
-- vs
-- pure f <*> x <*> y <*> z
-- (++) <$> Just "johntra" <*> Just "volta" -> Just "johntravolta" 

-- we can compose the IO actions
-- -- main = do  
--     a <- (++) <$> getLine <*> getLine  
--     putStrLn $ "The two lines concatenated turn out to be: " ++ a  

-- partly applied functions are applicative functors:
-- (+) <$> (+3) <*> (*100) $ 5 -> 8 + 500 = 508 



