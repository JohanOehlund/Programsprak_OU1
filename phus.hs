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


elem' :: (Eq a, Num t3, Num t2, Num t1, Num t) => [(a, Bool, (t, t1), (t2, t3))] -> a -> Bool
elem' [] reg1 = False
elem' ((reg0,_,(_,_),(_,_)):xs) reg1
    | reg0 == reg1 = True
    | otherwise = (elem') xs reg1   

phus :: [(String, Bool, (Integer, Integer))] -> [(String, Bool, (Integer, Integer))]
phus [] = []
--phus l =  phus_helper l []


phus_helper :: [(String, Bool, (Integer, Integer))] -> [(String, Bool, (Integer, Integer),(Integer, Integer))] -> 
    [(String, Bool, (Integer, Integer),(Integer, Integer))]
phus_helper [] output = output
phus_helper ((reg0,isParked0,(hour0,minute0)):xs) output
    | ((elem') output reg0) == True = phus_helper xs output
    | otherwise = phus_helper xs (((reg0,isParked0,(0,0),(hour0,minute0))) : output)



{-
elem2' :: (Eq a) => a -> [(a,Bool,(Integer,Integer),(Integer, Integer))] -> Bool 
elem2' reg1 [] = False 
elem2' y ((reg1,isParked1,(_,_),(hour1,minute1)):xs1) = foldl (\acc (reg1,_,(_,_),(_,_)) -> 
    if y /= reg1 then True else acc) False xs1  
-}