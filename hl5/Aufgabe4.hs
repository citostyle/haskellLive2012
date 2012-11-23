data Nat = Z | S Nat deriving Show

instance Eq Nat where
	(==) Z Z 			= True
	(==) (S m) Z		= False
	(==) Z (S n)		= False
	(==) (S m) (S n) 	= (m == n)

plusNat :: Nat -> Nat -> Nat
plusNat Z b = b
plusNat (S a) b = S (plusNat a b)

minusNat :: Nat -> Nat -> Nat
minusNat (S a) (S b) = minusNat a b
minusNat a Z = a
minusNat a (S b) = Z

timesNat :: Nat -> Nat -> Nat
timesNat a Z = Z
timesNat a (S b) = plusNat a (timesNat a b)


divNat :: Nat -> Nat -> Nat
divNat a Z = error "Invalid argument"
divNat Z b = Z
divNat a b
	| eqNat a b = (S Z)
	| grNat a b = plusNat (S Z) (divNat (minusNat a b) b)
	| otherwise = (Z)


modNat :: Nat -> Nat -> Nat
modNat _ Z = error "Invalid argument"
modNat Z _ = Z
modNat a b
	| eqNat a b = (Z)
	| grNat a b = modNat (minusNat a b) b
	| otherwise = a


eqNat :: Nat -> Nat -> Bool
eqNat Z Z = True
eqNat (S a) (S b) = eqNat a b
eqNat _ Z = False
eqNat Z _ = False


grNat :: Nat -> Nat -> Bool
grNat Z Z = False
grNat (S a) (S b) = grNat a b
grNat _ Z = True
grNat Z _ = False


leNat :: Nat -> Nat -> Bool
leNat Z Z = False
leNat (S a) (S b) = leNat a b
leNat _ Z = False
leNat Z _ = True


grEqNat :: Nat -> Nat -> Bool
grEqNat Z Z = True
grEqNat (S a) (S b) = grEqNat a b
grEqNat _ Z = True
grEqNat Z _ = False


leEqNat :: Nat -> Nat -> Bool
leEqNat Z Z = True
leEqNat (S a) (S b) = leEqNat a b
leEqNat _ Z = False
leEqNat Z _ = True

-- NatPair solutions

type NatPair = (Nat,Nat)

mkCan :: NatPair -> NatPair
mkCan (m, Z) 	 = (m, Z)
mkCan (Z, n)	 = (Z, n)
mkCan (S m, S n) = mkCan (m,n)

plusNP :: NatPair -> NatPair -> NatPair 
plusNP (m,n) (o,p) = mkCan ((plusNat m o), (plusNat n p))

minusNP :: NatPair -> NatPair -> NatPair 
minusNP (m,n) (o,p) =  mkCan ((plusNat m p), (plusNat n o))

timesNP :: NatPair -> NatPair -> NatPair 
timesNP (m,n) (o,p) = mkCan ((plusNat (timesNat m o) (timesNat n p)), (plusNat (timesNat m p) (timesNat n o)))

divNP :: NatPair -> NatPair -> NatPair
divNP (m,n) (o,p)
	| eqNat o p 					= error "Invalid argument"
	| eqNat m n 					= (Z,Z)
	| eqNP (m,n) (o,p)  			= ((S Z), Z)
	| (grNat m n) && (grNat o p) 	= ((divNat (minusNat m n) (minusNat o p)), Z)
	| (leNat m n) && (leNat o p) 	= ((divNat (snd (mkCan (m,n))) (snd (mkCan (o,p)))), Z)
	| (grNat m n) && (leNat o p) 	= (Z, S (divNat (minusNat m n) (snd (mkCan (o,p)))))
	| otherwise						= (Z, S (divNat (snd (mkCan (m,n))) (minusNat o p))) 

modNP :: NatPair -> NatPair -> NatPair
modNP mp np
	| eqNP (mkCan np) (Z,Z)	= error "Invalid argument"
 	| otherwise 			= minusNP mp (timesNP np (divNP mp np))

eqNP :: NatPair -> NatPair -> Bool 
eqNP mp np = eqNat (fst (mkCan mp)) (fst(mkCan np)) && eqNat (snd (mkCan mp))  (snd(mkCan np))

grNP :: NatPair -> NatPair -> Bool 
grNP (m,n) (o,p) = grNat (plusNat m p) (plusNat o n)

leNP :: NatPair -> NatPair -> Bool
leNP (m,n) (o,p) = leNat (plusNat m p) (plusNat o n)

grEqNP :: NatPair -> NatPair -> Bool
grEqNP (m,n) (o,p) = grEqNat (plusNat m p) (plusNat o n)

leEqNP :: NatPair -> NatPair -> Bool
leEqNP (m,n) (o,p) = leEqNat (plusNat m p) (plusNat o n)

absNP :: NatPair -> NatPair
absNP (Z,n) = (n, Z)
absNP (m,Z) = (m, Z)
absNP (m,n) = absNP (mkCan (m,n))
