import Register

phus :: [(String, Bool, (Integer, Integer))] ->  (String, [(String, (Integer, Integer))])
phus l = convertToOutput calcDayX maxParked []
        where calcDayX = mapTest l  $phus_helper l []
              maxParked = getMaxTimeParked calcDayX ("ABC123",(0,0))


elem' :: (Eq a, Num t3, Num t2, Num t1, Num t) => [(a, Bool, (t, t1), (t2, t3))] -> a -> Bool
elem' [] reg1 = False
elem' ((reg0,_,(_,_),(_,_)):xs) reg1
    | reg0 == reg1 = True
    | otherwise = (elem') xs reg1   

phus_helper :: [(String, Bool, (Integer, Integer))] -> [(String, Bool, (Integer, Integer),(Integer, Integer))] -> 
    [(String, Bool, (Integer, Integer),(Integer, Integer))]
phus_helper [] output = output
phus_helper ((reg0,isParked0,(hour0,minute0)):xs) output
    | ((elem') output reg0) == False = phus_helper xs (((reg0,isParked0,(0,0),(hour0,minute0))) : output)
    | otherwise = phus_helper xs output



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

convertToOutput :: [(String, Bool, (Integer, Integer),(Integer, Integer))] -> String -> [(String, (Integer, Integer))]  -> (String, [(String, (Integer, Integer))])
convertToOutput [] reg output = (reg,output)
convertToOutput ((reg1,isParked1,(cHour,cMinute),(hour1,minute1)):xs1) reg output =  convertToOutput xs1 reg ((reg1,(cHour,cMinute)) : output) 

getMaxTimeParked :: [(String, Bool, (Integer, Integer),(Integer, Integer))] -> (String,(Integer,Integer)) -> String
getMaxTimeParked [] (reg,(hour,minute)) = reg 
getMaxTimeParked ((reg1,isParked1,(cHour,cMinute),(hour1,minute1)):xs) (reg,(hour,minute)) = if ((cHour*60)+cMinute) > ((hour*60)+minute) then
                        getMaxTimeParked xs (reg1,(cHour,cMinute)) 
                     else 
                        getMaxTimeParked xs (reg,(hour,minute))