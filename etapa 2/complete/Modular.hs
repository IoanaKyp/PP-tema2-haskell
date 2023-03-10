module Modular where

import Data.List
import Data.Function (on)
import qualified Data.Set as S
import StandardGraph

type Graph a = StandardGraph a

{-
    O partiție este o mulțime de submulțimi ale unei alte mulțimi, disjuncte
    (fără elemente comune) și care împreună conțin toate elementele originale.
    
    De exemplu, pentru mulțimea [1,2,3], o posibilă partiție este [[1], [2,3]].

    Va fi folosită în etapa 3.
-}
type Partition a = S.Set (S.Set a)

{-
    *** TODO ***

    Aplică o funcție pe fiecare element al unei liste, însă doar pe unul singur
    la un moment dat, păstrându-le pe celalte nemodificate. Prin urmare, pentru
    fiecare element din lista inițială rezultă câte o listă în lista finală,
    aferentă modificării doar a acelui element.

    Exemplu:

    > mapSingle (+10) [1,2,3]
    [[11,2,3],[1,12,3],[1,2,13]]
-}

-- helper f xs acc = if null acc then [] else
--                     [xs ++ [f (head acc)] ++ (tail acc)] ++ helper f (xs ++ [head acc]) (tail acc)

-- mapSingle :: (a -> a) -> [a] -> [[a]]
-- mapSingle f xs = helper f [] xs

helper acc func l = if null l then [] else
                    [acc ++ [func (head l)] ++ (tail l)] ++ helper (acc ++ [head l]) func (tail l) 

mapSingle :: (a -> a) -> [a] -> [[a]]
mapSingle f xs = helper [] f xs



-- helper2 f xs acc = if null acc then [] else
--                     [xs ++ [f (head acc)] ++ (tail acc)] ++ helper2 f (xs ++ [head acc]) (tail acc)

-- mapSingle2 :: ([a] -> [a]) -> [a] -> [[a]]
-- mapSingle2 f xs = helper2 f [] xs

{-
    *** TODO ***x sssssssssssssssssssss xxxxxxxszsa

    Determină lista tuturor partițiilor unei liste. Deși mai sus tipul
    Partition a este definit utilizând mulțimi, aici utilizăm liste,
    pentru simplitate.

    Dacă vi se pare greu de urmărit tipul întors de funcție, cu 3 niveluri
    ale constructorului de tip listă, gândiți-vă așa:
    - avem nevoie de un nivel pentru o submulțime
    - încă un nivel pentru o partiție, care este o mulțime de submulțimi
    - încă un nivel pentru mulțimea tuturor partițiilor.

    Hint: Folosiți list comprehensions pentru a răspunde la întrebarea:
    dacă am obținut o partiție a restului listei, cum obținem o partiție
    a întregii liste, care include capul? (folosiți și mapSingle)

    Exemple:

    > partitions [2,3]
    [[[2],[3]],[[2,3]]]

    > partitions [1,2,3]
    [[[1],[2],[3]],[[1,2],[3]],[[2],[1,3]],[[1],[2,3]],[[1,2,3]]]
-}
partitions :: [a] -> [[[a]]]
partitions xs = --let subseq =   (Data.List.sortBy (\x y -> compare (length x) (length y)) ((Data.List.subsequences xs)))
--                     inv_subseq = reverse (Data.List.sortBy (\x y -> compare (length x) (length y)) ((Data.List.subsequences xs)))
                    --leftElem xs li = [ x | x<-xs, not x `elem` li ]
                -- in [[x] ++ y | x <- subseq, y <- (partitions [ z | z <- xs, not (z `elem` x)])]
    
    undefined --if length xs == 1 then [[xs]]
                -- else [ [[head xs]] ++ (head partitions(tail xs))]
    
    -- let singleNodes = mapSingle (\x -> x) xs
    --                 doubleNodes = [x ++ [y | y <- xs, y `notElem` x] | x <- singleNodes]

--     --             in [singleNodes ++ doubleNodes]
-- filter (\x -> length x <= 3) (Data.List.subsequences (Data.List.sortBy (\x y -> compare (length x) (length y)) (tail (Data.List.subsequences [1,2,3]))))
-- filter (\x -> length x <= 3) (Data.List.subsequences (reverse(Data.List.sortBy (\x y -> compare (length x) (length y)) (tail (Data.List.subsequences [1,2,3])))))
--  [x| [x, y]<- (filter (\x -> length x == 2) (Data.List.transpose [filter (\x -> length x <= 3) (Data.List.subsequences (Data.List.sortBy (\x y -> compare (length x) (length y)) (tail (Data.List.subsequences [1,2,3])))), filter (\x -> length x <= 3) (Data.List.subsequences (reverse(Data.List.sortBy (\x y -> compare (length x) (length y)) (tail (Data.List.subsequences [1,2,3])))))])), length x == 3]
--  filter (\x -> numofelem x == 3) (Data.List.subsequences (Data.List.sortBy (\x y -> compare (length x) (length y)) (tail (Data.List.subsequences [1,2,3]))))


-- idee: pentru fiecare submultime din lista o concatenam la toate submultimile rezultate din lista fara 
-- submultimea curenta pana cand obtinem lista vida

numofelem xs = if null xs then 0 else
    length(head xs) + numofelem (tail xs)

exists ::(Eq a) => a -> [[a]] -> Bool
exists n xs = if null xs then False else
              if n `elem` (head xs) then True else exists n (tail xs)

existsAll ns xs = if null ns then True else
                    if exists (head ns) xs then existsAll (tail ns) xs else False


deductList xs deduct rez =  if null xs then reverse rez else
                                if (head xs) `elem` deduct then deductList (tail xs) deduct rez
                                else deductList (tail xs) deduct  ((head xs) : rez)

-- partitionsHelp xs rez subseq = if null subseq then rez else
--                                partitionsHelp 
--                                     (deductList xs (head subseq))
--                                     (rez ++ (mapSingle2 ((head subseq) :) (Data.List.sortBy (\x y -> compare (length x) (length y)) ((Data.List.subsequences (deductList xs (head subseq))))) )) 
--                                     (tail subseq)

-- partitions xs = partitionsHelp xs [] (Data.List.sortBy (\x y -> compare (length x) (length y)) ((Data.List.subsequences xs)))