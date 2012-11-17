
-- Diese Lösung basiert darauf, dass die Lampe am Ende genau dann leuchtet
-- wenn der Lichtschalter ungerade oft betätigt wurde

licht :: Integer -> Bool
licht n = odd $ length [x | x <- [1..n], n `mod` x == 0]


