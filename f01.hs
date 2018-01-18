-- Enkel funktion
f x = x + 1

-- Rekursiv summafunktion (en av raderna är inte nödvändig, vilken?)
sumlist [] = 0
sumlist [h] = h
sumlist (h:t)  = h + sumlist t

-- Hjälpfunktion för sumlist2, första argumentet är en ackumulator
sumlist2' s [] = s
sumlist2' s (h:t) = sumlist2' (s + h) t

-- Summafunktion som använder hjälpfunktion
sumlist2 l = sumlist2' 0 l

-- Hjälpfunktion för reverseList, första argumentet samlar listan i omvänd ordning (hur?)
reverseList' l1 [] = l1
reverseList' l1 (h:t) = reverseList' (h:l1) t

-- Vänd på en lista
reverseList l = reverseList' [] l


lucky :: (Integral a) => a -> String  
lucky 7 = "LUCKY NUMBER SEVEN!"  
lucky x = "Sorry, you're out of luck, pal!"

charName :: String -> String  
charName "a" = "Albert"  
charName "b" = "Broseph"  
charName "c" = "Cecil"

addVectors :: (Num a) => (a, a) -> (a, a) -> (a, a)  
addVectors (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)
