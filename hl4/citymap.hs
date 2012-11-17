import Data.Maybe
import Data.List
import qualified Data.Map as Map

type Bezirk 	= Integer
type AnzBezirke = Integer
type Route 		= (Bezirk,Bezirk)  -- neighbouring districts
newtype CityMap = CM (AnzBezirke,[Route])

test = CM (6,[(2,3),(1,2),(3,1),(4,3),(4,5),(5,6),(6,3)])
test2 = CM (7,[(2,3),(1,2),(3,1),(4,3),(4,5),(5,6),(6,3),(5,7)])


neighbours :: CityMap -> Bezirk -> [Bezirk]
neighbours (CM (_,rs)) d = mapMaybe neighbour rs
	where neighbour (p,q)
			| d == p    = Just q
			| d == q    = Just p
			| otherwise = Nothing

type Pfad = [Bezirk]

paths :: CityMap -> Bezirk -> Bezirk -> [Pfad]
--paths cm start goal = paths' [] cm start goal
paths = paths' []
	where paths' visited cm start goal
			| start == goal 		  = [start:[]]
			| start `notElem` visited = [start:rest | next <- neighbours cm start,
				 			  						  rest <- paths' (start:visited) cm next goal]
			| otherwise = []

---------------------------------------------------------------------------
type NeighbourFunction = (Bezirk -> [Bezirk])

paths2 :: NeighbourFunction -> Bezirk -> Bezirk -> [Pfad]
paths2 neighbours start goal
	| start == goal = [start:[]]
	| otherwise     = [start:rest | next <- neighbours start,
									rest <- paths2 neighbours' next goal]
					   where 
					   	 neighbours' = filter (/= start) . neighbours
----------------------------------------------------------------------------

type Bottleneck = Bezirk

bottlenecks :: CityMap -> Bezirk -> Bezirk -> [Bottleneck]
bottlenecks cm@(CM (n,rs)) start goal = foldl intersect ([1..n] \\ [start,goal]) $
									   	paths cm start goal
									 -- paths2 (neighbours rs) start goal


type BottleneckCount = Integer
type PfadGrenzen = (Bezirk,Bezirk)

allBottlenecks :: CityMap -> (BottleneckCount, [(Bottleneck,[PfadGrenzen])])
allBottlenecks cm@(CM (n,_)) = (bCount, bList)
	where
		bCount = fromIntegral $ length bList
		bList = Map.toList . Map.fromListWith (++) $
				[(b,[(p,q)]) | p <- [1..n], q <- [p..n], p /= q,
							   b <- bottlenecks cm p q]

