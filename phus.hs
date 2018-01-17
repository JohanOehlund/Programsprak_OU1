lucky :: (Integral a) => a -> String  
lucky 7 = "LUCKY NUMBER SEVEN!"  
lucky x = "Sorry, you're out of luck, pal!"

charName :: String -> String  
charName "a" = "Albert"  
charName "b" = "Broseph"  
charName "c" = "Cecil"

addVectors :: (Num a) => (a, a) -> (a, a) -> (a, a)  
addVectors (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

phus' l1 [] =l1
phus' l1 (h:t) = phus' (h:l1) t

phus :: [(String, Bool, (Integer, Integer))] ->
  (String, [(String, (Integer, Integer))])
phus [(regNumber,isParked,(hour,minute))]= if isParked then(regNumber,[(regNumber,(hour,minute))]) else error "No one parked today..." 
--phus :: [Char]-> [Char] 
--phus l = phus'[] l