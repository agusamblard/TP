module City ( City, newC, nameC, distanceC )
   where

import Point (Point, newP, difP)

data City = Cit String Point deriving (Eq, Show)

newC :: String -> Point -> City
newC = Cit


nameC :: City -> String
nameC (Cit city point) = city


distanceC :: City -> City -> Float
distanceC (Cit city1 point1) (Cit city2 point2) = difP point1 point2