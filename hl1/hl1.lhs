Einfache Addition zweier Zahlen:
Erster Parameter ist somit vom Type Integer, der zweite ebenfalls, der Rueckgabewert der Funktion ist auch vom Typ Integer

> myadd :: Integer -> Integer -> Integer -- Signatur
> myadd a b = a + b

'myadd' kann auch Infix verwendet werden:
5 `myadd` 5

Funktion, welche eine andere Funktion verwendet

> addSeven :: Integer -> Integer
> addSeven a = myadd a 7

Beispiel fuer Pattern-Matching, achten auf die Reihenfolge

> eqSeven :: Integer -> Bool
> eqSeven 7 = True
> eqSeven _ = False

Alternative Implementierung unter Verwendung von if

> eqSeven2 a = 
>   if a == 7 then 
>      True 
>   else 
>      False

Noch eine Alternative, 'otherwise' ist ein Alias fuer True

> eqSeven3 a
>   | a == 7 = True
>   | otherwise = False


Summiert die Elemente einer Integer-Liste

> mysum :: [Integer] -> Integer
> mysum [] = 0 -- Abbruchbedingung der Rekursion
> mysum (x:rest) = x + (mysum rest)


Alternative Implementierung mit der vordefinierten Funktion 'sum'

> mysum2 list = sum list


Addiert die Zahl 'a' zu jedem Listenelement

> addX :: [Integer] -> Integer -> [Integer]
> addX [] _ = []
> addX (x:rest) a = (x + a) : (addX rest a)

Alternativimplementierung unter Verwendung von List-Comprehension

> addX_lc list a = [x + a | x <- list]

Alternative mit der Funktion 'map'
Fuer jedes Listenelement 'x' aus 'list' wird die Operation 'x + a' ausgefuehrt,
das Ergebnis bildet wieder eine Liste

> addX_map list a = map (+a) list

Konstruiertes und sinnfreies Beispiel um den Umgang von Integer und Int zu zeigen.
Die Funktion 'length' liefert die Laenge der Liste als Typ Int, 'sum' allerdings
berechnet die Summe als Typ Integer. Zur Konvertierung wird die Funktion 'fromIntegral'
eingesetzt.

> sumPlusLength :: [Integer] -> Integer
> sumPlusLength list = summe + len
>   where
>      len = fromIntegral (length list)
>      summe = sum list


Grosse Zahl bei Integer

> big :: Integer
> big = 1000*100000000000*10000000

Grosse Zahl bei Int

> big2 :: Int
> big2 = 1000*10000000000*10000000
