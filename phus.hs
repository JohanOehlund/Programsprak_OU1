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
    | ((elem') output reg0) == False = phus_helper xs (((reg0,isParked0,(0,0),(hour0,minute0))) : output)
    | isParked0 == True =  phus_helper xs output
    | isParked0 == False = phus_helper xs output
    | otherwise = []



test :: [(String, Bool, (Integer, Integer))] -> (String, Bool, (Integer, Integer),(Integer, Integer)) -> 
    (String, Bool, (Integer, Integer),(Integer, Integer))
test [] output =output
test ((reg0,isParked0,(hour0,minute0)):xs) (reg1,isParked1,(cHour,cMinute),(hour1,minute1))
    | reg0 == reg1 && isParked1==True && isParked0 == False = test xs (reg1,isParked0,(diffTime (cHour,cMinute) (hour1,minute1) (hour0,minute0)),(hour0,minute0))
    | reg0 == reg1 && isParked1==False && isParked0 ==True = test xs (reg1,isParked0,(cHour,cMinute),(hour0,minute0))
    |otherwise = test xs (reg1,isParked1,(cHour,cMinute),(hour1,minute1))

diffTime :: (Integer,Integer) -> (Integer,Integer) -> (Integer,Integer)-> (Integer,Integer)
diffTime (currHour,currMinute) (checkInHour,checkInMinute) (checkOutHour,checkOutMinute) =
     (((((abs)(((checkOutHour*60)+checkOutMinute)-((checkInHour*60)+checkInMinute))+currMinute) `div` 60)+currHour),((abs)(checkOutHour*60+checkOutMinute)-(checkInHour*60+checkInMinute)+currMinute) `mod` 60)


mapTest :: [(String, Bool, (Integer, Integer))] -> [(String, Bool, (Integer, Integer),(Integer, Integer))] -> 
    [(String, Bool, (Integer, Integer),(Integer, Integer))]
mapTest _ [] = []
mapTest dayX ((reg1,isParked1,(cHour,cMinute),(hour1,minute1)):xs) = test dayX (reg1,isParked1,(cHour,cMinute),(hour1,minute1)) : mapTest dayX xs


getMaxTimeParked :: [(String, Bool, (Integer, Integer),(Integer, Integer))] -> (String,(Integer,Integer)) -> String
getMaxTimeParked [] (reg,(hour,minute)) = reg 
getMaxTimeParked ((reg1,isParked1,(cHour,cMinute),(hour1,minute1)):xs) (reg,(hour,minute)) = if ((cHour*60)+cMinute) > ((hour*60)+minute) then
                        getMaxTimeParked xs (reg1,(cHour,cMinute)) 
                     else 
                        getMaxTimeParked xs (reg,(hour,minute))

{-

| checkInMinute+checkOutMinute >=60 = (((((abs)((checkOutHour*60+checkOutMinute)-(checkInHour*60+checkInMinute))+currHour*60) `div` 60)), (((abs)((checkOutHour*60+checkOutMinute)-(checkInHour*60+checkInMinute))+currMinute)`mod`60))
    | 

diffTime :: (Integer,Integer) -> (Integer,Integer) -> (Integer,Integer)-> (Integer,Integer)
diffTime (currHour,currMinute) (checkInHour,checkInMinute) (checkOutHour,checkOutMinute) 
    | checkInMinute+checkOutMinute >= 60 && checkOutHour-checkInHour < 24 = ((currHour+abs(checkOutHour-checkInHour)-1),(currMinute+ abs((checkOutMinute+checkInMinute)-60)))
    | otherwise = ((currHour+abs(checkOutHour-checkInHour)),(currMinute+abs(checkOutMinute-checkInMinute)))

checkIn :: (String,(Integer,Integer)) -> [(String, Bool, (Integer, Integer),(Integer, Integer))] ->
    (String, Bool, (Integer, Integer),(Integer, Integer))
checkIn (reg0,(hour0,minute0)) ((reg1,isParked1,(cHour,cMinute),(hour1,minute1)):xs)
    | reg0 ==reg1 = (reg1,isParked1,(cHour+hour0,cMinute+minute0),(hour1,minute1))
    | otherwise = checkIn (reg0,(hour0,minute0)) xs

split :: [(String,Bool,(Integer,Integer))] -> ([(String,Bool)], [(Integer,Integer)])
split [] = ([], [])
split [(reg,isParked,(hour,minute))] = ([(reg,isParked)], [(hour,minute)])
addTime :: (Eq a, Num t3, Num t2, Num t1, Num t) => [(a, Bool, (t, t1), (t2, t3))] -> (a, Bool, (t, t1)) ->
    [(a, Bool, (t, t1), (t2, t3))]
addTime [] (reg0,isParked0,(hour0,minute0)) = []
addTime ((reg1,isParked1,(cHour,cMinute),(_,_)):xs) (reg0,isParked0,(hour0,minute0))
    | reg1 == reg0 = (reg1,isParked1,((cHour+hour0),(cMinute+minute0)),(hour0,minute0))
    | otherwise = addTime xs (reg0,isParked0,(hour0,minute0))
-}


{-
elem2' :: (Eq a) => a -> [(a,Bool,(Integer,Integer),(Integer, Integer))] -> Bool 
elem2' reg1 [] = False 
elem2' y ((reg1,isParked1,(_,_),(hour1,minute1)):xs1) = foldl (\acc (reg1,_,(_,_),(_,_)) -> 
    if y /= reg1 then True else acc) False xs1  
-}