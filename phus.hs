import Register
import Data.List
--phus' :: [(String, Bool, (Integer, Integer))] -> (String, Bool, (Integer, Integer),(Integer, Integer)) ->
--[(String, Bool, (Integer, Integer),(Integer, Integer))]

elem' :: (Eq a) => a -> [a] -> Bool  
elem' a [] = False  
elem' a (x:xs)  
    | a == x    = True  
    | otherwise = a `elem'` xs


phus' :: [(String, Bool, (Integer, Integer))] -> [(String, Bool, (Integer, Integer),(Integer, Integer))] -> 
 [(String, Bool, (Integer, Integer),(Integer, Integer))] 
--phus' [] [] = error "Can't call head on an empty list!"  
phus' ((reg,isParked,(hour,minute)):t) output = phus' t output <- [(reg,isParked,(0,0),(hour,minute))]


--phus' ((reg,isParked,(hour,minute)):t) output = if reg `elem` output then 
 --[reg,isParked,(0,0),(hour,minute)] else [reg,isParked,(0,0),(hour,minute)]
  
--phus' input output=if length input ==0  then output 
--("DGH739",True,(7,15))

--phus :: [(String, Bool, (Integer, Integer))] -> (String, [(String, (Integer, Integer))])
phus l = phus' l []

phusOutput output = ("HEJ", [("Monica",(13,37))])


head' :: [a] -> a  
head' [] = error "Can't call head on an empty list, dummy!"  
head' (x:_) = x 

--tail' :: [a] -> [a]  
--tail' [] = error "Can't call tail on an empty list, dummy!"  
--tail' (_:x) = x  