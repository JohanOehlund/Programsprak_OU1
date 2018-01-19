-- Det n:te fibonaccitalet
fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib 2 = (fib 1) + (fib 0)
fib x = (fib (x - 1)) + (fib (x - 2))

-- Alla fibonaccital
fibs = map fib [1..]

-- fst, fast annars
first :: (a, b) -> a
first (x,_) = x

-- Första tre elementen på en lista - fungerar inte på listor med färre
-- element
firstThree :: [a] -> (a,a,a)
firstThree (x:y:z:_) = (x,y,z)

-- If-satsillustration
f :: Int -> String
f x = if x > 0 then
      "kaka"
    else
       "kex"

-- Guards
g :: Int -> Int -> String
g x y
  | a > 0     = "kaka" 
  | a < 0     = "kex"
  | otherwise = "boll" 
  where a = x - y

-- Let
h :: Int -> Int -> (Int,Int)
h x y = let b = x - y
   in 
     (b, b*b)

-- Case
i :: [(String, Int, (Int, Int))] -> Int
i [] = 0
i (h:_) = case h of
  (_,_, (x,_)) -> x


-- Typdiskussion runt hjälpfunktioner
--phus :: [(String, Bool, (Int, Int))] -> (String, [(String, (Int, Int))])

--phus_helper :: 

--reverse' :: [a] -> [a]
--reverse' l = let (x, _) = reverse_helper [] l
--             in x

--reverse_helper' [a] -> [a] -> ([a], Int)
