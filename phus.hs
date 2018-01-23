import Register
type CarparkInfoList = [(String, Bool, (Integer, Integer))]
type CarparkOutputTuple = (String, [(String, (Integer, Integer))])
type TimeParked = (Integer,Integer)

{-
Function: phus
Comment: Calculates total time parked for each car in the carpark.
-}
phus :: CarparkInfoList ->  CarparkOutputTuple
phus l = convertToOutput calcDayX maxParked []
        where calcDayX = (map') l  $phus_helper l []
              maxParked = getMaxTimeParked calcDayX ("ABC123",(0,0))

{-
Function: phus_helper
Comment: Checks if a car is an element of a list.
-}
phus_helper :: CarparkInfoList -> [(String, Bool, TimeParked,TimeParked)] -> 
    [(String, Bool, TimeParked,TimeParked)]
phus_helper [] output = output
phus_helper ((reg0,isParked0,(hour0,minute0)):xs) output
    | ((elem') output reg0) == False = phus_helper xs (((reg0,isParked0,(0,0),(hour0,minute0))) : output)
    | otherwise = phus_helper xs output


{-
Function: elem'
Comment: Checks if a car is an element of a list.
-}
elem' :: (Eq a, Num t3, Num t2, Num t1, Num t) => [(a, Bool, (t, t1), (t2, t3))] -> a -> Bool
elem' [] reg1 = False
elem' ((reg0,_,(_,_),(_,_)):xs) reg1
    | reg0 == reg1 = True
    | otherwise = (elem') xs reg1   


{-
Function: map'
Comment: converted map function inorder to apply every calcTimeDiffs function to each element of the list from
phus_helper.
-}
map' :: CarparkInfoList -> [(String, Bool, TimeParked,TimeParked)] -> 
    [(String, Bool, TimeParked,TimeParked)]
map' _ [] = []
map' dayX ((reg1,isParked1,(cHour,cMinute),(hour1,minute1)):xs) = calcTimeDiffs dayX (reg1,isParked1,(cHour,cMinute),(hour1,minute1)) : map' dayX xs



{-
Function: calcTimeDiffs
Comment: Calculates the total time parked for each car in the carpark.
-}
calcTimeDiffs :: CarparkInfoList -> (String, Bool, TimeParked,TimeParked) -> 
    (String, Bool, TimeParked,TimeParked)
calcTimeDiffs [] output =output
calcTimeDiffs ((reg0,isParked0,(hour0,minute0)):xs) (reg1,isParked1,(cHour,cMinute),(hour1,minute1))
    | reg0 == reg1 && isParked1==True && isParked0 == False = calcTimeDiffs xs (reg1,isParked0,(diffTime (cHour,cMinute) (hour1,minute1) (hour0,minute0)),(hour0,minute0))
    | reg0 == reg1 && isParked1==False && isParked0 ==True = calcTimeDiffs xs (reg1,isParked0,(cHour,cMinute),(hour0,minute0))
    | otherwise = calcTimeDiffs xs (reg1,isParked1,(cHour,cMinute),(hour1,minute1))

{-
Function: diffTime
Comment: Calculates the time parked for a single car in the carpark.
-}
diffTime :: TimeParked -> TimeParked -> TimeParked-> TimeParked
diffTime (currHour,currMinute) (checkInHour,checkInMinute) (checkOutHour,checkOutMinute) =
     (((((abs)(((checkOutHour*60)+checkOutMinute)-((checkInHour*60)+checkInMinute))+currMinute) `div` 60)+currHour),((abs)(checkOutHour*60+checkOutMinute)-(checkInHour*60+checkInMinute)+currMinute) `mod` 60)

{-
Function: convertToOutput
Comment: Converts the output from getMaxTimeParked and map' inorder to match the expected output of the function phus.
-}
convertToOutput :: [(String, Bool, TimeParked,TimeParked)] -> String -> [(String, TimeParked)]  -> CarparkOutputTuple
convertToOutput [] reg output = (reg,output)
convertToOutput ((reg1,isParked1,(cHour,cMinute),(hour1,minute1)):xs1) reg output =  convertToOutput xs1 reg ((reg1,(cHour,cMinute)) : output) 

{-
Function: getMaxTimeParked
Comment: Returns the car with the maximum time parked in the carpark.
-}
getMaxTimeParked :: [(String, Bool, TimeParked,TimeParked)] -> (String,TimeParked) -> String
getMaxTimeParked [] (reg,(hour,minute)) = reg 
getMaxTimeParked ((reg1,isParked1,(cHour,cMinute),(hour1,minute1)):xs) (reg,(hour,minute)) = if ((cHour*60)+cMinute) > ((hour*60)+minute) then
                        getMaxTimeParked xs (reg1,(cHour,cMinute)) 
                     else 
                        getMaxTimeParked xs (reg,(hour,minute))