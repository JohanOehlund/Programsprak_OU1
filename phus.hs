import Register
import Data.List
{-
phus_filter :: (Eq a) => [(a, Bool, (Integer, Integer))] -> [(a, Bool, (Integer, Integer))] -> Bool
phus_filter (reg0,_,(_,_):_) (reg1,_,(_,_):_) = if reg0 == reg1 then True else False

phus' :: [(String, Bool, (Integer, Integer))] -> [(String, Bool, (Integer, Integer),(Integer,Integer))] ->
 (String, [(String, (Integer, Integer))])
phus' [] [] = error "Empty list!"

phus :: [(String, Bool, (Integer, Integer))] -> (String, [(String, (Integer, Integer))])
phus l = phus' l []
-}

filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' f (x:xs)
  | f x       = x : (filter' f xs)
  | otherwise = filter' f xs




phus :: [(String, Bool, (Integer, Integer))] -> [(String, Bool, (Integer, Integer),(Integer, Integer))]
phus [] = []
phus (x:xs) =  phus_helper x [] ++ phus xs 

phus_helper :: (String, Bool, (Integer, Integer)) -> [(String, Bool, (Integer, Integer),(Integer, Integer))] -> 
    [(String, Bool, (Integer, Integer),(Integer, Integer))]
--phus_helper x [] = []
phus_helper (reg0,isParked0,(hour0,minute0)) output
    | isParked0 == True = (reg0,isParked0,(0,0),(hour0,minute0)) : output
    | isParked0 == False = (reg0,isParked0,(2,2),(hour0,minute0)) : output 