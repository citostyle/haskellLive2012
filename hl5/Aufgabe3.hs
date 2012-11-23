import Data.Char
import Data.List

type NegaBinary = String

extract :: String -> NegaBinary
extract str 
   | null conv = "0"
   | otherwise = conv
   where
      conv = (dropWhile (=='0') . filter (\x -> x == '0' || x =='1')) str

cutZeros :: String -> String
cutZeros str
   | null conv = "0"
   | otherwise = conv
   where
      conv = dropWhile (=='0') str 


nbIncr :: String -> String
nbIncr z = cutZeros $ reverse $ incr2 (reverse z) 

nbDecr :: NegaBinary -> NegaBinary
nbDecr z = cutZeros $ reverse $ decr2 (reverse z)

decr2 :: NegaBinary -> NegaBinary
decr2 [] = "11"
decr2 ('1':x) = '0' : x
decr2 ('0':x) = '1' : (incr2 x)
-- letzte stelle wird 1, dh. zahl z um 1 groesser
-- x stellt -z/2 dar und wird um 1 erhoeht, dh. (-z/2 + 1)
-- gesamt: (-z/2 + 1)*(-2) + 1 = z - 2 + 1 = z - 1

incr2 :: NegaBinary -> NegaBinary
incr2 [] = "1"
incr2 ('0':x) = '1' : x
incr2 ('1':x) = '0' : (decr2 x)
-- letzte stelle wird 0, dh.: zahl z um 1 kleiner
-- x stellt -z/2 dar und wird um 1 reduziert, dh. (-z/2 - 1)
-- gesamt: (-z/2 - 1)*(-2) - 1 = z + 2 - 1 = z + 1


-- nbIncr/nbDecr mit wikipedia-variante
nbIncr' z = cutZeros $ reverse $ incr (reverse z) 1
nbDecr' z = cutZeros $ reverse $ incr (reverse z) (-1)

--variante wikipedia mit carry
incr :: NegaBinary -> Integer -> NegaBinary
incr [] 0 = ""
incr [] 1 = "1"
incr [] (-1) = '1' : (incr "0" 1)
incr ('0':x) 0 = '0' : (incr x 0)
incr ('0':x) 1 = '1' : (incr x 0)
incr ('0':x) (-1) = '1' : (incr x 1)
incr ('1':x) 0 = '1' : (incr x 0)
incr ('1':x) 1 = '0' : (incr x (-1))
incr ('1':x) (-1) = '0' : (incr x 0)


nbAbs :: NegaBinary -> NegaBinary
nbAbs nb
   | (negative nb) = nbAbs' "0" nb
   | otherwise = nb

nbAbs' :: NegaBinary -> NegaBinary -> NegaBinary
nbAbs' nb "0" = nb
nbAbs' n1 n2 = nbAbs' (nbIncr n1) (nbIncr n2)

-- gerade anzahl an stellen -> positiv
positive :: NegaBinary -> Bool
positive = odd . genericLength . cutZeros

-- ungerade anzahl an stellen -> negativ
negative :: NegaBinary -> Bool
negative = not . positive


nbPlus :: NegaBinary -> NegaBinary -> NegaBinary
nbPlus a "0" = a
nbPlus "0" b = b
nbPlus a b
   | (negative b) = nbPlus (nbDecr a) (nbIncr b)
   | otherwise = nbPlus (nbIncr a) (nbDecr b)


nbTimes :: NegaBinary -> NegaBinary -> NegaBinary
nbTimes a "0" = "0"
nbTimes "0" b = "0"
nbTimes a "1" = a
nbTimes "1" b = b
nbTimes a b
   | negative a && negative b = nbTimes (nbAbs a) (nbAbs b)
   | negative b = nbPlus b (nbTimes b (nbDecr a))
   | otherwise = nbPlus a (nbTimes a (nbDecr b))
