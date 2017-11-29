
module Basics where
import Data.List  
import qualified Data.Map as Map  

-- functions
doubleMe x = x + x
doubleUs x y = doubleMe x  + doubleMe y
doubleSmallNumber x = if x > 100 then x else x * 2

-- even variables are just functions with no arg
name = "Pavlo"

-- lists
lostNumbers = [4, 8, 15, 16, 23, 42]
-- to concat ++: [1, 2, 4] ++ [4, 5, 6] or "pavlo" ++ "pohrebnyi" -> "pavlopohrebnyi"
-- to prepend one char : (called "cons"): 'a' : "pavlo" -> "apavlo"
-- to get an element by index !!: [1, 2, 3] !! 0  -> 1
-- nested: [[1, 2, 3], [4, 5, 6]]
-- get a head: head [1, 2 ,3] -> 1
-- get a tail: tail [1, 2 ,3] -> [2, 3]
-- get a last el: last [1, 2 ,3] -> 3
-- init: init [1, 2 ,3] -> [1, 2]
-- take: take 2 [1, 2, 3] -> [1, 2]
-- take: drop 2 [1, 2, 3] -> [1]
-- minimum: minimum [1, 2, 3] -> 1
-- maximum: maximum [1, 2, 3] -> 3
-- sum, product
-- contains: elem 3 [1, 2, 3] or 3 `elem` [1, 2, 3] -> True
-- range: [1..20], [2, [4..20]
-- 24 elements of infinite list: take 24 [1, 2..]
-- cycle: take 12 (cycle "LOL ") -> "LOL LOL LOL "
-- repeat: take 10 (repeat 5) -> [5, 5, 5, 5, 5, 5, 5, 5, 5, 5]
-- replicate: replicat:e 10 5 -> [5, 5, 5, 5, 5, 5, 5, 5, 5, 5]
-- comprehensions: [x * 2 | x <- [1..10], x <= 5] -> [2, 4, 6, 8, 10]
listLength xs = sum [1 | _ <- xs]

-- tuples
t = ("pavlo", 27)
-- get first: fst (1, 2) -> 1
-- get second: scd (1, 2) -> 2
-- zip [1..3] ["one", "two", "three"] -> [(1, "one", ...)] 

--types
removeNonUppercase :: [Char] -> [Char]
removeNonUppercase st = [c | c <- st, c `elem` ['A'..'Z']]
-- 3 args
addThree :: Int -> Int -> Int -> Int
addThree x y z = x + y + z

-- typeclasses
-- A typeclass is a sort of interface that defines some behavior. 
-- If a type is a part of a typeclass, that means that it supports 
-- and implements the behavior the typeclass describes. 
-- Examples: Eq, Ord, Show, Read, Enum, Bounded, Num

-- pattern matching
-- 1
lucky :: (Integral a) => a -> String
lucky 7 = "LUCKU NUMBER SEVEN!"
lucky x = "Sorry, you are out of luck, pal!"
-- 2
factorial :: (Integral a) => a -> a  
factorial 0 = 1  
factorial n = n * factorial (n - 1)  
-- 3
addVectors :: (Num a) => (a, a) -> (a, a) -> (a, a)  
addVectors (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)  
-- 4
head' :: [a] -> a  
head' [] = error "Can't call head on an empty list, dummy!"  
head' (x:xs) = x 
-- 5
tell :: (Show a) => [a] -> String  
tell [] = "The list is empty"   
tell (x:[]) = "The list has one element: " ++ show x  
tell (x:y:[]) = "The list has two elements: " ++ show x ++ " and " ++ show y  
tell (x:y:_) = "This list is long. The first two elements are: " ++ show x ++ " and " ++ show y
-- 6
length' :: (Num b) => [a] -> b  
length' [] = 0  
length' (_:xs) = 1 + length' xs   
-- 7
sum' :: (Num a) => [a] -> a 
sum' [] = 0
sum' (x:xs) = x + sum' xs
-- 8 as pattern (@)
capital :: String -> String  
capital "" = "Empty string, whoops!"  
capital all@(x:xs) = "The first letter of " ++ all ++ " is " ++ [x]  

-- guards
-- 1
bmiTell :: (RealFloat a) => a -> String  
bmiTell bmi  
    | bmi <= 18.5 = "You're underweight!"  
    | bmi <= 25.0 = "You're supposedly normal. Pffft, I bet you're ugly!"  
    | bmi <= 30.0 = "You're fat! Lose some weight, fatty!"  
    | otherwise   = "You're a whale, congratulations!"  
-- 2
bmiTell' :: (RealFloat a) => a -> a -> String  
bmiTell' weight height  
    | bmi <= skinny = "You're underweight, you emo, you!"  
    | bmi <= normal = "You're supposedly normal. Pffft, I bet you're ugly!"  
    | bmi <= fat    = "You're fat! Lose some weight, fatty!"  
    | otherwise     = "You're a whale, congratulations!"  
    where bmi = weight / height ^ 2  
          skinny = 18.5  
          normal = 25.0  
          fat = 30.0  
-- 3
max' :: (Ord a) => a -> a -> a  
max' a b   
    | a > b     = a  
    | otherwise = b  
-- 4
myCompare :: (Ord a) => a -> a -> Ordering  
a `myCompare` b  
    | a > b     = GT  
    | a == b    = EQ  
    | otherwise = LT  
-- 5 function in where
calcBmis :: (RealFloat a) => [(a, a)] -> [a]  
calcBmis xs = [bmi w h | (w, h) <- xs]  
    where bmi weight height = weight / height ^ 2  

-- let - used for local val and func bindings
-- 1 just in any place
-- 4 * (let a = 9 in a + 1) + 2  
-- 2 multiple
---(let a = 100; b = 200; c = 300 in a*b*c, let foo="Hey "; bar = "there!" in foo ++ bar)
-- 3 with comprehentions
calcBmis' :: (RealFloat a) => [(a, a)] -> [a]  
calcBmis' xs = [bmi | (w, h) <- xs, let bmi = w / h ^ 2]  

-- case
head2 :: [a] -> a  
head2 xs = case xs of [] -> error "No head for empty lists!"  
                      (x:_) -> x  
-- 1 applied in any place of the func
describeList :: [a] -> String  
describeList xs = "The list is " ++ case xs of [] -> "empty."  
                                               [x] -> "a singleton list."   
                                               xs -> "a longer list." 
-- 2 it is just like a pattern mathing inside the func
elem2 :: (Eq a) => a -> [a] -> Bool
elem2 _ [] = False 
elem2 x xs = case (find (==x) xs) of Just _ -> True
                                     Nothing -> False

-- recursion
-- 1
maximum' :: (Ord a) => [a] -> a  
maximum' [] = error "maximum of empty list"  
maximum' [x] = x  
maximum' (x:xs)   
    | x > maxTail = x  
    | otherwise = maxTail  
    where maxTail = maximum' xs  
-- 2
replicate' :: (Num i, Ord i) => i -> a -> [a]  
replicate' n x  
    | n <= 0    = []  
    | otherwise = x:replicate' (n-1) x  
-- 3 it is infinite so we can use -> take 5 (repeat 3)
repeat' :: a -> [a]  
repeat' x = x:repeat' x  
-- 4
zip' :: [a] -> [b] -> [(a,b)]  
zip' _ [] = []  
zip' [] _ = []  
zip' (x:xs) (y:ys) = (x,y):zip' xs ys  
-- 5
elem' :: (Eq a) => a -> [a] -> Bool  
elem' a [] = False  
elem' a (x:xs)  
    | a == x    = True  
    | otherwise = a `elem'` xs   
-- 6
quicksort :: (Ord a) => [a] -> [a]  
quicksort [] = []  
quicksort (x:xs) =   
    let smallerSorted = quicksort [a | a <- xs, a <= x]  
        biggerSorted = quicksort [a | a <- xs, a > x]  
    in  smallerSorted ++ [x] ++ biggerSorted  

-- higher-order func
-- 1
multThree :: (Num a) => a -> a -> a -> a  
multThree x y z = x * y * z  
-- 2
compareWithHundred :: (Num a, Ord a) => a -> Ordering  
compareWithHundred = compare 100  
-- 3 func as an arg -> applyTwice (+3) 10 
-- NOTE: (+3) is partially applied so it results in a func: Num a => a -> a 
applyTwice :: (a -> a) -> a -> a  
applyTwice f x = f (f x) 
-- 4 example: zipWith' (+) [4,2,5,6] [2,6,2,3] -> [5, 7, 9]
zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]  
zipWith' _ [] _ = []  
zipWith' _ _ [] = []  
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys  
-- 5
flip' :: (a -> b -> c) -> b -> a -> c  
flip' f y x = f x y  
-- 6 example: map' (+3) [1,5,3,1,6]  
map' :: (a -> b) -> [a] -> [b]  
map' _ [] = []  
map' f (x:xs) = f x : map' f xs 
-- 7 example: filter (>3) [1,5,3,2,1,6,4,3,2,1] 
-- example: let notNull x = not (null x) in filter notNull [[1,2,3],[],[3,4,5],[2,2],[],[],[]],[1,2,3],[3,4,5],[2,2]]  
filter' :: (a -> Bool) -> [a] -> [a]  
filter' _ [] = []  
filter' p (x:xs)   
    | p x       = x : filter' p xs  
    | otherwise = filter' p xs   
-- 8 example: chain 10 -> [10,5,16,8,4,2,1]  
chain :: (Integral a) => a -> [a]  
chain 1 = [1]  
chain n  
    | even n =  n:chain (n `div` 2)  
    | odd n  =  n:chain (n*3 + 1) 

-- lamndas
-- 1, the lambda: \xs -> length xs > 15
numLongChains :: Int  
numLongChains = length (filter (\xs -> length xs > 15) (map chain [1..100]))  

-- folds
sum2 :: (Num a) => [a] -> a  
sum2 xs = foldl (\acc x -> acc + x) 0 xs 
-- or shorter: sum2 xs = foldl (+) 0 xs 

-- func application, used for parentheses free code
-- ($) :: (a -> b) -> a -> b  
-- sqrt (3 + 4 + 9) --> sqrt $ 3 + 4 + 9
-- f (g (z x)) --> f $ g $ z x
-- sum (filter (> 10) (map (*2) [2..10])) --> sum $ filter (> 10) $ map (*2) [2..10]

-- func composition
-- (.) :: (b -> c) -> (a -> b) -> a -> c  
-- example: f . g = \x -> f (g x)  
-- example: negate . (* 3)
-- we want to make all numbers absolute and then negate them
-- 1: map (\x -> negate (abs x)) [5,-3,-6,7,-3,2,-19,24] 
-- 2: map (negate . abs) [5,-3,-6,7,-3,2,-19,24]  

-- Maps (dictionaries)
-- example: 
-- import qualified Data.Map as Map 
-- Map.fromList [("betty","555-2938"),("bonnie","452-2928")]

-- Sets
-- example
-- import qualified Data.Set as Set  
-- Set.fromList [1,2,3]

-- data types
-- 1 way (constructor function):
data Point = Point Float Float deriving (Show)
data Shape = Circle Point Float | Rectangle Point Point deriving (Show)  
surface :: Shape -> Float  
surface (Circle _ r) = pi * r ^ 2  
surface (Rectangle (Point x1 y1) (Point x2 y2)) = (abs $ x2 - x1) * (abs $ y2 - y1)  
-- example: surface (Rectangle (Point 0 0) (Point 100 100))  
-- 2 way (record syntax):
data Person = Person { firstName :: String  
                     , lastName :: String  
                     , age :: Int  
                     } deriving (Show, Eq, Read)   
-- example1: Person {firstName="Pavlo", lastName="Pohrebnyi", age=27}
-- example2: Person "Pavlo" "Pohrebnyi" 27
-- example3: (Person "Pavlo" "Pohrebnyi" 27) == (Person "Pavlo" "Pohrebnyi" 28) 
-- example4: read "Person {firstName = "Pavlo", lastName = "Pohrebnyi", age = 27}" :: Person

-- type parameters (a - type parameter)
data Vector a = Vector a a a deriving (Show)  
vplus :: (Num t) => Vector t -> Vector t -> Vector t  
(Vector i j k) `vplus` (Vector l m n) = Vector (i+l) (j+m) (k+n) 

-- deriving all reserved typeclasses
data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday   
           deriving (Eq, Ord, Show, Read, Bounded, Enum)   

-- type synonims
type String' = [Char] 
-- example1:
type PhoneNumber = String  
type Name = String  
type PhoneBook = [(Name, PhoneNumber)]  
inPhoneBook :: Name -> PhoneNumber -> PhoneBook -> Bool  
inPhoneBook name pnumber pbook = (name, pnumber) `elem` pbook 
data Either' a b = Left' a | Right' b deriving (Eq, Ord, Read, Show)  
-- example2:
data LockerState = Taken | Free deriving (Show, Eq)  
type Code = String  
type LockerMap = Map.Map Int (LockerState, Code)  
lockerLookup :: Int -> LockerMap -> Either String Code  
lockerLookup lockerNumber map =   
    case Map.lookup lockerNumber map of   
        Nothing -> Left $ "Locker number " ++ show lockerNumber ++ " doesn't exist!"  
        Just (state, code) -> if state /= Taken   
                                then Right code  
                                else Left $ "Locker " ++ show lockerNumber ++ " is already taken!"  
lockers :: Map.Map Int (LockerState, Code) 
lockers = Map.fromList   
    [(100,(Taken,"ZD39I"))  
    ,(101,(Free,"JAH3I"))  
    ,(103,(Free,"IQSA9"))  
    ,(105,(Free,"QOTSA"))  
    ,(109,(Taken,"893JJ"))  
    ,(110,(Taken,"99292"))  
    ] 
-- lockerLookup 101 lockers
-- example 4
data List2 a = Empty2 | Cons2 {listHead :: a, listTail :: List2 a} 
    deriving (Show, Read, Eq, Ord)
-- 1 `Cons2` (2 `Cons2` Empty2)
-- example 5
data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)

singletonTree :: a -> Tree a  
singletonTree x = Node x EmptyTree EmptyTree  

treeInsert :: (Ord a) => a -> Tree a -> Tree a  
treeInsert x EmptyTree = singletonTree x  
treeInsert x (Node a left right)   
    | x == a = Node x left right  
    | x < a  = Node a (treeInsert x left) right  
    | x > a  = Node a left (treeInsert x right) 
-- foldr treeInsert EmptyTree [4,3,6,1,4,7,8]    

treeElem :: (Ord a) => a -> Tree a -> Bool  
treeElem x EmptyTree = False  
treeElem x (Node a left right)  
    | x == a = True  
    | x < a  = treeElem x left  
    | x > a  = treeElem x right  

-- typeclasses 2
-- example 1
data TrafficLight = Red | Yellow | Green  
-- we use instanse to make our TrafficLight type an instance of Eq typeclass
-- class Eq a where  
--     (==) :: a -> a -> Bool  
--     (/=) :: a -> a -> Bool  
--     x == y = not (x /= y)  
--     x /= y = not (x == y)  
instance Eq TrafficLight where  
    Red == Red = True  
    Green == Green = True  
    Yellow == Yellow = True  
    _ == _ = False  
instance Show TrafficLight where  
    show Red = "Red light"  
    show Yellow = "Yellow light"  
    show Green = "Green light"  
-- instance (Eq m) => Eq (Maybe m) where  
--     Just x == Just y = x == y  
--     Nothing == Nothing = True  
--     _ == _ = False  
-- example 2
class YesNo a where  
    yesno :: a -> Bool
instance YesNo Int where  
    yesno 0 = False  
    yesno _ = True 
instance YesNo [a] where  
    yesno [] = False  
    yesno _ = True 
instance YesNo Bool where  
    yesno = id  -- id: std identity function a -> a
instance YesNo (Maybe a) where  
    yesno (Just _) = True  
    yesno Nothing = False
instance YesNo (Tree a) where  
    yesno EmptyTree = False  
    yesno _ = True  
instance YesNo TrafficLight where  
    yesno Red = False  
    yesno _ = True
yesnoIf :: (YesNo y) => y -> a -> a -> a  
yesnoIf yesnoVal yesResult noResult = if yesno yesnoVal then yesResult else noResult  
-- yesnoIf [2,3,4] "YEAH!" "NO!" 

-- functors
-- insight:
-- class Functor f where  
--     fmap :: (a -> b) -> f a -> f b 
-- instance Functor Maybe where  
--     fmap f (Just x) = Just (f x)  
--     fmap f Nothing = Nothing  
-- example 1:
-- fmap (++ " HEY GUYS IM INSIDE THE JUST") (Just "Something serious.")  
-- example 2:
instance Functor Tree where  
    fmap f EmptyTree = EmptyTree  
    fmap f (Node x leftsub rightsub) = Node (f x) (fmap f leftsub) (fmap f rightsub) 









