--Krypto Kracker

import Data.List

--phrase von der bekannt ist, dass sie im text vorkommt
clearphrase = "the quick brown fox jumps over the lazy dog"

--verschluesselter text
secret = ["vtz ud xnm xugm itr pyy jttk gmv xt otgm xt xnm puk ti xnm fprxq",
          "xnm ceuob lrtzv ita hegfd tsmr xnm ypwq ktj",
          "frtjrpgguvj otvxmdxd prm iev prmvx xnmq"]

type Entry = (Char,Char) -- (from,to)
type Dict = [Entry]

--liefert jene textpassagen, welche sich fuer einen mustervergleich eignen
--dafuer muss die laenge uebereinstimmen
possiblePhrases :: [String] -> String -> [String]
possiblePhrases text phrase =
   [x | x <- text, (length x) == (length phrase)]


--fuehrt den mustervergleich durch und baut den schluessel (Dict) auf
trySubstitution :: String -> String -> Dict -> Dict
trySubstitution [] [] key = key
trySubstitution (c:cypher) (t:clear) dict 
   | in_dict = trySubstitution cypher clear dict
   | no_single = trySubstitution cypher clear ((c,t):dict)
   | otherwise = []
   where
      in_dict = containsEntry dict (c,t) --Entry bereits im Dictionary
      no_single = (not $ containsKey dict c) && (not $ containsValue dict t)
      -- weder Key, noch Value sind im Dictionary schon vorhanden


--entschluesselt text anhand von schluessel
decrypt :: Dict -> [String] -> [String]
decrypt [] text = []
decrypt dict text = [[ getValue dict char | char <- line ] | line <- text]

--ausgangspunkt
crack :: [String] -> String -> [[String]]
crack text clear = 
   [decrypt key text | key  <- keys, (length key) > 0]
   where
      phrases = allPossiblePhrases text clear --allPossiblePhrases text clear
      keys = nub [trySubstitution phrase clear [] | phrase <- phrases]


--hilfsfunktionen fuer verwendetes dictionary:
--existiert ein entry im dictionary?
containsEntry :: Dict -> Entry -> Bool
containsEntry dict e@(from,to) = elem e dict

--existiert ein eintrag mit entsprechendem key
containsKey :: Dict -> Char -> Bool
containsKey [] _ = False
containsKey dict from = or $ map(\(x,_) -> x==from) dict

--existiert ein eintrag mit entsprechendem value
containsValue :: Dict -> Char -> Bool
containsValue [] _ = False
containsValue dict to = or $ map(\(_,y) -> y==to) dict

--liefert den value zu einem eintrag mit entsprechendem key
getValue :: Dict -> Char -> Char
getValue dict from = (snd . head . filter (\(x,y) -> x==from)) dict


{-
#####################################################################################
-}

--erweiterung:
--liefert alle moeglichen phrasen, zeilenunabhaengig
allPossiblePhrases :: [String] -> String -> [String]
allPossiblePhrases text phrase = phrases
   where
      len = length $ words phrase -- laenge der klartext-phrase
      comp = words (concat $ intersperse " " text) -- generiert eine liste alle woerter
      -- des verschluesselten textes
      count = length comp --anzahl der woerter im text

      phrases = nub $ concat [(map (unwords) . filter (\x -> (length x) == len)) (subPhrase t len) | pos <- [0..count-len], let t = drop pos comp ]

--teilt eine liste von woertern in listen der laenge 'len' auf
subPhrase :: [String] -> Int -> [[String]]
subPhrase [] len = []
subPhrase words len = front : (subPhrase back len)
   where
      (front,back) = splitAt len words

--klartextphrase kommt nun nicht mehr als ganze zeile vor
secret2 = ["vtz ud xnm xugm itr pyy jttk gmv xt otgm",
          "xt xnm puk ti xnm fprxq",
          "xnm ceuob lrtzv ita hegfd",
          "tsmr xnm ypwq ktj frtjrpgguvj otvxmdxd",
          "prm iev prmvx xnmq"]

