module Link ( Link, newL, linksL, connectsL, capacityL, delayL )
   where

import Point (Point, newP)
import City (City, newC, nameC)
import Quality (Quality, newQ, capacityQ, delayQ)

data Link = Lin City City Quality deriving (Eq, Show)

newL :: City -> City -> Quality -> Link -- genera un link entre dos ciudades distintas
newL = Lin

connectsL :: City -> Link -> Bool   -- indica si esta ciudad es parte de este link
connectsL city (Lin city1 city2 quality) = city == city1 || city == city2

linksL :: City -> City -> Link -> Bool -- indica si estas dos ciudades distintas estan conectadas mediante este link
linksL city1 city2 (Lin city3 city4 quality) = (city1 == city3 && city2 == city4) || (city1 == city4 && city2 == city3)

capacityL :: Link -> Int
capacityL (Lin city1 city2 int) = capacityQ int

delayL :: Link -> Float     -- la demora que sufre una conexion en este canal
delayL (Lin city1 city2 float) = delayQ float

