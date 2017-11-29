-- Reverse Polish notation calculator
-- 10 4 3 + 2 * - == 10 - (4 + 3) * 2
solveRPN :: (Num a, Read a) => String -> a
solveRPN expression = head (foldl foldStack [] (words expression))
    where foldStack (x:y:ys) "*" = (x * y):ys
          foldStack (x:y:ys) "+" = (x + y):ys
          foldStack (x:y:ys) "-" = (x - y):ys
          foldStack xs number = (read number):xs
