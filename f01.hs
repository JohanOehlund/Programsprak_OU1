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
