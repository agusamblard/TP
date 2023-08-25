module Region ( Region, newR, foundR, linkR, tunelR, connectedR, linkedR, delayR, availableCapacityForR )
   where

import Point (Point, newP)
import City (City, newC, nameC)
import Quality (Quality, newQ, capacityQ, delayQ)
import Link (Link, linksL, delayL, newL, capacityL)
import Tunel (Tunel, newT, connectsT, usesT, delayT)
import GHC.Exts.Heap (GenClosure(tsoStack))

data Region = Reg [City] [Link] [Tunel] deriving (Show)

newR :: Region
newR = Reg [] [] []

foundR :: Region -> City -> Region -- agrega una nueva ciudad a la región
foundR (Reg cs ls ts) city
    | city `elem` cs = error "Esta ciudad ya forma parte de la region"
    | otherwise = Reg (cs ++ [city]) ls ts

hasLinkR :: [Link] -> City -> City -> Bool
hasLinkR [] _ _ = False
hasLinkR (l:ls) c1 c2
    | linksL c1 c2 l = True
    | otherwise = hasLinkR ls c1 c2

linkR :: Region -> City -> City -> Quality -> Region -- enlaza dos ciudades de la región con un enlace de la calidad indicada
linkR (Reg cs ls ts) city1 city2 quality
    | hasLinkR ls city1 city2 = error "Ya hay un link que enlaza estas ciudades"
    |not (city1 `elem` cs && city2 `elem` cs) = error "Una de las ciudades ingresadas no pertenece a esta región."
    | otherwise =
        let newLink = newL city1 city2 quality
        in Reg cs (ls ++ [newLink]) ts


tunelR :: Region -> [City] -> Region
tunelR (Reg [] _ _) _ = error "No hay ciudades en la Región"
tunelR (Reg cs1 ls ts) cs2
    | not (all (`elem` cs1) cs2) = error "No están todas las ciudades en la Región"
    | not (linksEachCity cs2 ls) = error "No están enlazadas todas las ciudades"
    | alreadyHasTunel newTunel ts = error "Ya existe este Tunel"
    | otherwise = Reg cs1 ls (ts ++ [newTunel])
    where
        linksEachCity :: [City] -> [Link] -> Bool
        linksEachCity [] _ = False
        linksEachCity (c1:c2:cs) ls
            | hasLinkR ls c1 c2 = True
            | otherwise = linksEachCity (c2:cs) ls

        whichLinks :: [City] -> [Link] -> [Link]
        whichLinks [] _ = []
        whichLinks [_] _ = []
        whichLinks (c1:c2:cs) ls
            | hasLinkR ls c1 c2 = linkBCities c1 c2 ls : whichLinks (c2:cs) ls
            | otherwise = whichLinks (c2:cs) ls

        linkBCities :: City -> City -> [Link] -> Link
        linkBCities c1 c2 (l:ls)
            | linksL c1 c2 l = l
            | otherwise = linkBCities c1 c2 ls

        alreadyHasTunel :: Tunel -> [Tunel] -> Bool
        alreadyHasTunel _ [] = False
        alreadyHasTunel newTunel (t:ts)
            | newTunel == t = True
            | otherwise = alreadyHasTunel newTunel ts

        newLinks = whichLinks cs2 ls
        newTunel = newT newLinks

connectedR :: Region -> City -> City -> Bool -- indica si estas dos ciudades estan conectadas por un tunel
connectedR (Reg _ _ []) city1 city2 = False
connectedR (Reg cs ls (t:ts)) city1 city2
    | connectsT city1 city2 t = True
    | otherwise = connectedR (Reg cs ls ts) city1 city2

linkedR :: Region -> City -> City -> Bool -- indica si estas dos ciudades estan enlazadas
linkedR (Reg cs [] ts) city1 city2 = False
linkedR (Reg cs (l:ls) ts) city1 city2
    | linksL city1 city2 l = True
    | otherwise = linkedR (Reg cs ls ts) city1 city2

delayR :: Region -> City -> City -> Float -- dadas dos ciudades conectadas, indica la demora
delayR (Reg _ [] []) _ _ =  0.0
delayR (Reg cs ls ts) city1 city2
    | not (city1 `elem` cs && city2 `elem` cs) = error "Una de las ciudades ingresadas no pertenece a esta región."  
    |otherwise = 
    let totalOfTunel = delayT (theTunel (Reg cs ls ts) city1 city2)
    in totalOfTunel
    where
        theTunel :: Region -> City -> City -> Tunel
        theTunel (Reg cs ls (t:ts)) city1 city2
            | connectsT city1 city2 t = t
            | otherwise = theTunel (Reg cs ls ts) city1 city2 

availableCapacityForR :: Region -> City -> City -> Int -- indica la capacidad disponible entre dos ciudades
availableCapacityForR (Reg _ [] _) city1 city2 = error "No hay links"
availableCapacityForR (Reg cs _ _) city1 city2
    |not (city1 `elem` cs && city2 `elem` cs) = error "Una de las ciudades ingresadas no pertenece a esta región." 
availableCapacityForR reg@(Reg cs (l:ls) ts) city1 city2 =
    if linksL city1 city2 l then
        capacityL l - usedCapacity l reg
    else
        availableCapacityForR (Reg cs ls ts) city1 city2

usedCapacity :: Link -> Region -> Int
usedCapacity l (Reg _ _ ts) = capacityL l - tunelsLink l ts

tunelsLink :: Link -> [Tunel] -> Int
tunelsLink _ [] = 0
tunelsLink l (t:ts)
    | usesT l t = 1 + tunelsLink l ts
    | otherwise = tunelsLink l ts