module Point ( Point, newP, difP)
   where

data Point = Poi Int Int deriving (Eq, Show)

newP :: Int -> Int -> Point
newP = Poi -- el data Point toma dos Int que representan las coordenadas x e y de un punto 

difP :: Point -> Point -> Float  -- distancia absoluta
difP (Poi x1 y1) (Poi x2 y2) =
    let dx = fromIntegral (x2 - x1)
        dy = fromIntegral (y2 - y1)
    in sqrt (dx^2 + dy^2)


