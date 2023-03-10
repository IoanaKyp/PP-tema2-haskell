{-# LANGUAGE TupleSections #-}
module StandardGraph where

import qualified Data.Set as S

{-
    Graf ORIENTAT cu noduri de tipul a, reprezentat prin mulțimile (set)
    de noduri și de arce.

    Mulțimile sunt utile pentru că gestionează duplicatele și permit
    testarea egalității a două grafuri fără a ține cont de ordinea nodurilor
    și a arcelor.

    type introduce un sinonim de tip, similar cu typedef din C.
-}
type StandardGraph a = (S.Set a, S.Set (a, a))

data GraphData a = MakeGraf
    { nodesData :: (S.Set a)
    , edgesData :: (S.Set (a, a))
    } deriving Show

{-
    *** TODO ***

    Construiește un graf pe baza listelor de noduri și de arce.

    Hint: S.fromList.

    Constrângerea (Ord a) afirmă că valorile tipului a trebuie să fie
    ordonabile, lucru necesar pentru reprezentarea internă a mulțimilor.
    Este doar un detaliu, cu care nu veți opera explicit în această etapă.
    Veți întâlni această constrângere și în tipurile funcțiilor de mai jos.
-}
fromComponents :: Ord a
               => [a]              -- lista nodurilor
               -> [(a, a)]         -- lista arcelor
               -> StandardGraph a  -- graful construit
fromComponents ns es = (S.fromList ns, S.fromList es)

fromComponentsData :: Ord a
                   => [a]
                   -> [(a,a)]
                   -> GraphData a
fromComponentsData ns es = MakeGraf (S.fromList ns) (S.fromList es)

-- constructorii nodesData si edgesData pot fi folositi ca fuctii echivalente cu nodes si edges 
-- din versiunea initiala.

{-
    *** TODO ***

    Mulțimea nodurilor grafului.
-}
nodes :: StandardGraph a -> S.Set a
nodes = fst

{-
    *** TODO ***

    Mulțimea arcelor grafului.
-}
edges :: StandardGraph a -> S.Set (a, a)
edges = snd

{-
    Exemple de grafuri
-}
graph1 :: StandardGraph Int
graph1 = fromComponents [1, 2, 3, 3, 4] [(1, 2), (1, 3), (1, 2)]

graph2 :: StandardGraph Int
graph2 = fromComponents [4, 3, 3, 2, 1] [(1, 3), (1, 3), (1, 2)]

graph3 :: StandardGraph Int
graph3 = fromComponents [1, 2, 3, 4] [(1, 2), (1, 4), (4, 1), (2, 3), (1, 3)]

graph4 :: StandardGraph Int
graph4 = fromComponents [1, 2, 3, 4] [(1, 2), (1, 4), (4, 1), (2, 4), (1, 3)]

shouldBeTrue :: Bool
shouldBeTrue = graph1 == graph2

{-
    *** TODO ***

    Mulțimea nodurilor înspre care pleacă arce dinspre nodul curent.

    Exemplu:

    > outNeighbors 1 graph3
    fromList [2,3,4]
-}
outNeighbors :: Ord a => a -> StandardGraph a -> S.Set a
outNeighbors node graph = S.fromList [snd x | x <- S.toList (edges graph), fst x == node]

{-
    *** TODO ***

    Mulțimea nodurilor dinspre care pleacă arce înspre nodul curent.

    Exemplu:

    > inNeighbors 1 graph3 
    fromList [4]
-}
inNeighbors :: Ord a => a -> StandardGraph a -> S.Set a
inNeighbors node graph = S.fromList [fst x | x <- S.toList (edges graph), snd x == node]

{-
    *** TODO ***

    Întoarce graful rezultat prin eliminarea unui nod și a arcelor în care
    acesta este implicat. Dacă nodul nu există, întoarce același graf.

    Exemplu:

    > removeNode 1 graph3
    (fromList [2,3,4],fromList [(2,3)])
-}
removeNode :: Ord a => a -> StandardGraph a -> StandardGraph a
removeNode node graph = (S.fromList [x | x <- S.toList (nodes graph), not (x == node)] , S.fromList [x | x <- S.toList (edges graph), not(fst x == node || snd x == node)] )

{-
    *** TODO ***

    Divizează un nod în mai multe noduri, cu eliminarea nodului inițial.
    Arcele în care era implicat vechiul nod trebuie să devină valabile
    pentru noile noduri.

    Exemplu:

    > splitNode 2 [5,6] graph3
    (fromList [1,3,4,5,6],fromList [(1,3),(1,4),(1,5),(1,6),(4,1),(5,3),(6,3)])
-}
splitNode :: Ord a
          => a                -- nodul divizat
          -> [a]              -- nodurile cu care este înlocuit
          -> StandardGraph a  -- graful existent
          -> StandardGraph a  -- graful obținut
-- splitNode old news graph =(removeNode old (S.fromList(news ++ S.toList(nodes graph)),
--                                            S.fromList ([(x,y) | x <- news, y <- S.toList(outNeighbors old graph)] 
--                                             ++ [(y,x) | x <- news, y <- S.toList(inNeighbors old graph)] 
--                                             ++ S.toList(edges graph)) ))

splitNode old news graph = let  allNodes = S.fromList(news ++ S.toList(nodes graph))
                                newOutEdge = [(x,y) | x <- news, y <- S.toList(outNeighbors old graph)]
                                newInEdge = [(y,x) | x <- news, y <- S.toList(inNeighbors old graph)]
                                originalNodes = S.toList(edges graph)
                                allEdges = S.fromList(newOutEdge ++ newInEdge ++ originalNodes)
                           in (removeNode old (allNodes, allEdges))                                 

{-
    *** TODO ***

    Îmbină mai multe noduri într-unul singur, pe baza unei proprietăți
    respectate de nodurile îmbinate, cu eliminarea acestora. Arcele în care
    erau implicate vechile noduri vor referi nodul nou.

    Exemplu:

    > mergeNodes even 5 graph3
    (fromList [1,3,5],fromList [(1,3),(1,5),(5,1),(5,3)])
-}
mergeNodes :: Ord a
           => (a -> Bool)      -- proprietatea îndeplinită de nodurile îmbinate
           -> a                -- noul nod
           -> StandardGraph a  -- graful existent
           -> StandardGraph a  -- graful obținut
mergeNodes prop node graph = let toBeEliminated = [x | x <- S.toList (nodes graph), prop x == True]
                                 outEnds = S.toList (S.fromList [snd x | x <- S.toList (edges graph), fst x `elem` toBeEliminated])
                                 inEnds = S.toList (S.fromList[fst x | x <- S.toList (edges graph), snd x `elem` toBeEliminated])
                                 hasCircleEdge = if null [x | x <- S.toList (edges graph), fst x `elem` toBeEliminated && snd x `elem` toBeEliminated] then False else True
                                 allNodes = node : S.toList(nodes graph)
                                 allEdges = [(node,y) | y <- outEnds] ++ [(x,node) | x <- inEnds] ++ S.toList(edges graph)
                                 finalNodes = filter (\x -> not (x `elem` toBeEliminated)) allNodes
                                 leftEdges = filter (\x -> not(fst x `elem` toBeEliminated || snd x `elem` toBeEliminated)) allEdges
                                 finalEdges = if hasCircleEdge then [(node, node)] ++ leftEdges else leftEdges
                             in if null toBeEliminated then graph else (S.fromList finalNodes, S.fromList finalEdges)


-- mergeNodes prop node graph =  if nothingToDo then graph else (S.fromList newNodes, S.fromList newEdges)
--                             where
--                                 -- fac o lista cu ce am de sters
--                                 toBeDeleted = [x | x <- S.toList (nodes graph), prop x == True]
--                                 -- toBeDeleted = filter (\x -> (prop x) == True)
--                                 -- pentru toate nodurile de sters vad ce muchii ies din ele si pastrez nodurile destinatie
--                                 fromDeleted = S.toList (S.fromList [snd x | x <- S.toList (edges graph), fst x `elem` toBeDeleted])
--                                 -- pentru toate nodurile de sters vad ce muchii intra in ele si pastrez nodurile sursa
--                                 toDeleted = S.toList (S.fromList[fst x | x <- S.toList (edges graph), snd x `elem` toBeDeleted])
--                                 -- verific daca exista o muchie intre 2 noduri de sters 
--                                 hasCircleEdge = if null [x | x <- S.toList (edges graph), fst x `elem` toBeDeleted && snd x `elem` toBeDeleted] then False else True
--                                 -- toate nodurile
--                                 allNodes = node : S.toList(nodes graph)
--                                 -- toate muchiile
--                                 allEdges = [(node,y) | y <- fromDeleted] ++ [(x,node) | x <- toDeleted] ++ S.toList(edges graph)
--                                 -- nodurile finale
--                                 newNodes = [x | x <- allNodes, not (x `elem` toBeDeleted)]
--                                 -- muchiile ramase (mai putin cazurile de x->x)
--                                 leftEdges = [x | x <- allEdges, not(fst x `elem` toBeDeleted || snd x `elem` toBeDeleted)]
--                                 -- muchiile finale
--                                 newEdges = if hasCircleEdge then [(node, node)] ++ leftEdges else leftEdges
--                                 nothingToDo = null toBeDeleted