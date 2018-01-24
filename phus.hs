import Register
type CarparkInfoList = [(String, Bool, (Integer, Integer))]
type CarparkOutputTuple = (String, [(String, (Integer, Integer))])
type TimeParked = (Integer,Integer)

{-
Function: phus
Comment: Calculates total time parked for each car in the carpark.
-}
phus :: CarparkInfoList ->  CarparkOutputTuple
phus l = (fst (head calcDayX),calcDayX)
        where calcDayX = (quicksort) $(map') l
{-
Function: notElem'
Comment: Checks if a car is not an element of a list.
-}
notElem' :: (Eq a, Num t1, Num t) => (a,Bool,(t, t1)) -> a -> Bool
notElem' (reg0,_,(_,_)) reg1
    | reg0 /= reg1 = True 
    | otherwise = False

{-
Function: map'
Comment: converted map function inorder to apply every calcTimeDiffs function to each element of the list from
phus_helper.
-}
map' :: CarparkInfoList -> [(String,TimeParked)]
map' [] = []
map' ((reg,isParked,(hour,minute)):xs) = (calcTimeDiffs ((reg,isParked,(hour,minute)):xs) (reg,isParked,(hour,minute),(0,0))) : map' [x|x <- xs ,(notElem') x reg] 

{-
Function: calcTimeDiffs
Comment: Calculates the total time parked for each car in the carpark.
-}
calcTimeDiffs :: CarparkInfoList -> (String,Bool,TimeParked,TimeParked) -> (String,TimeParked)
calcTimeDiffs [] (reg,_,(hour,minute),(totHour,totMinute)) =(reg,(totHour,totMinute))
calcTimeDiffs ((reg0,isParked0,(hour0,minute0)):xs) (reg1,isParked1,(hour1,minute1),(totHour,totMinute)) 
    | reg0 == reg1 && isParked1==True && isParked0 == False = calcTimeDiffs xs (reg1,isParked0,(hour0,minute0),(diffTime (totHour,totMinute) (hour1,minute1) (hour0,minute0)))
    | reg0 == reg1 && isParked1==False && isParked0 ==True = calcTimeDiffs xs (reg1,isParked0,(hour0,minute0),(totHour,totMinute))
    | otherwise = calcTimeDiffs xs (reg1,isParked1,(hour1,minute1),(totHour,totMinute))

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
convertToOutput :: [(String,TimeParked)] -> String -> [(String, TimeParked)]  -> CarparkOutputTuple
convertToOutput [] reg output = (reg,output)
convertToOutput ((reg1,(totHour,totMinute)):xs1) reg output =  convertToOutput xs1 reg ((reg1,(totHour,totMinute)) : output) 

{-
Function: quicksort
Comment: Sorts the list so that the car with longest parking time is first.
-}
quicksort :: [(String,TimeParked)] -> [(String,TimeParked)]
quicksort [] = []  
quicksort (x:xs) =
  (quicksort [a|a<-xs, parkedLonger a x]) ++ [x] ++ (quicksort [a|a<-xs, parkedLonger x a])

{-
Function: parkedLonger
Comment: Checks if a car has parked longer than another car.
-}
parkedLonger :: (String,TimeParked) -> (String,TimeParked) -> Bool
parkedLonger (_,(totHour0,totMinute0)) (_,(totHour1,totMinute1)) 
            | (totHour0,totMinute0) > (totHour1,totMinute1) = True
            | otherwise = False
