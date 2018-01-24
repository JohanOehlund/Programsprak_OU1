import Register
type CarparkInfoList = [(String, Bool, (Integer, Integer))]
type CarparkOutputTuple = (String, [(String, (Integer, Integer))])
type TimeParked = (Integer,Integer)

{-
Function: phus
Comment: Calculates total time parked for each car in the carpark.
-}
phus :: CarparkInfoList ->  CarparkOutputTuple
phus l =(fst(head(calcDayX)),calcDayX)
        where calcDayX = (map') l []
{-
Function: notElem'
Comment: Checks if a car is not an element of a list.
-}
notElem' :: (Eq a, Num t3, Num t2, Num t1, Num t) => [(a,(t, t1),(t2,t3))] -> a -> Bool
notElem' [] reg = True
notElem' ((reg0,(_,_),(_,_)):xs) reg1
    | reg0 /= reg1 = notElem' xs reg1 
    | otherwise = False

{-
Function: quicksort
Comment: Sorts the list so that the car with longest parking time is first.
-}
quicksort :: [(String,TimeParked)] -> [(String,TimeParked)]
quicksort [] = []  
quicksort (x:xs) =
  (quicksort [a|a<-xs, parkedLonger a x]) ++ [x] ++ (quicksort [a|a<-xs, parkedLonger x a])

{-
Function: convertToOutput
Comment: Converts the calculated list of total parking times to the expected output of phus.
-}
convertToOutput :: [(String,TimeParked,TimeParked)] -> [(String,TimeParked)]
convertToOutput [] = []
convertToOutput ((reg,(hour,minute),(totHour,totMinute)):xs) = (reg,(totHour,totMinute)):convertToOutput xs

{-
Function: map'
Comment: converted map function inorder to apply every calcTimeDiffs function to each element of the list from
phus_helper.
-}
map' :: CarparkInfoList -> [(String,TimeParked,TimeParked)] -> [(String,TimeParked)]
map' [] output = ((quicksort) $((convertToOutput) output))
map' ((reg,isParked,(hour,minute)):xs) output 
    | (notElem') output reg == True =  map' xs ((reg,(hour,minute),(0,0)):output)
    | isParked == False = map' xs (changeTimeParked (reg,(hour,minute)) [] output)
    | isParked == True =  map' xs (changeCheckInTime (reg,(hour,minute)) [] output)
    | otherwise = map' xs output

{-
Function: diffTime
Comment: Calculates the time parked for a single car in the carpark.
-}
diffTime :: TimeParked -> TimeParked -> TimeParked-> TimeParked
diffTime (currHour,currMinute) (checkInHour,checkInMinute) (checkOutHour,checkOutMinute) =
     (((((abs)(((checkOutHour*60)+checkOutMinute)-((checkInHour*60)+checkInMinute))+currMinute) `div` 60)+currHour),
        ((abs)(checkOutHour*60+checkOutMinute)-(checkInHour*60+checkInMinute)+currMinute) `mod` 60)

{-
Function: parkedLonger
Comment: Checks if a car has parked longer than another car.
-}
parkedLonger :: (String,TimeParked) -> (String,TimeParked) -> Bool
parkedLonger (_,(totHour0,totMinute0)) (_,(totHour1,totMinute1)) 
            | (totHour0,totMinute0) > (totHour1,totMinute1) = True
            | otherwise = False

            {-
Function: changeCheckInTime
Comment: Changes the check in time for a car in a list (the list which is used to calculate the total parking time).
-}
changeCheckInTime :: (String,TimeParked) -> [(String,TimeParked,TimeParked)] -> [(String,TimeParked,TimeParked)] -> [(String,TimeParked,TimeParked)]
changeCheckInTime (reg1,(totHour1,totMinute1)) l ((reg,(hour,minute),(totHour,totMinute)):xs)
            | reg == reg1 =l++ ((reg,(totHour1,totMinute1),(totHour,totMinute)):xs)
            | otherwise = changeCheckInTime (reg1,(totHour1,totMinute1)) ((reg,(hour,minute),(totHour,totMinute)):l) xs
{-
Function: changeTimeParked
Comment: Changes the total parking time for a car in a list (the list which is used to calculate the total parking time).
-}
changeTimeParked :: (String,TimeParked) -> [(String,TimeParked,TimeParked)] -> [(String,TimeParked,TimeParked)] -> [(String,TimeParked,TimeParked)]
changeTimeParked (reg1,(totHour1,totMinute1)) l ((reg,(hour,minute),(totHour,totMinute)):xs)
            | reg == reg1 = l++ ((reg,(totHour1,totMinute1),(diffTime (totHour,totMinute) (hour,minute) (totHour1,totMinute1))):xs)
            | otherwise = changeTimeParked (reg1,(totHour1,totMinute1)) ((reg,(hour,minute),(totHour,totMinute)):l) xs
