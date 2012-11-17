
-- Soll der Schalter der Lampe l im Durchgang i gedrückt werden?
shouldSwitch :: Integer -> Integer -> Bool
shouldSwitch l i = l `mod` i == 0

-- Alle Schalterbetätigungen der Lampe l nach n Durchgängen
switchHistory :: Integer -> Integer -> [Bool]
switchHistory l n = [shouldSwitch l i | i <- [1..n]]

-- Ist die Lampe l mit Anfangszustand s nach n Durchgängen an oder aus?
isLampOn :: Integer -> Bool -> Integer -> Bool
isLampOn l s n = foldl xor s (switchHistory l n)
	where
		xor a True  = not a
		xor a False = a

-- Ist in einem Korridor mit n ausgeschalteten Lampen nach n Durchgängen 
-- des Nachtwächters die letzte Lampe an oder aus?
isLastLampOn :: Integer -> Bool
isLastLampOn n = isLampOn n False n
