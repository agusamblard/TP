module Quality ( Quality, newQ, capacityQ, delayQ )
   where

data Quality = Qua String Int Float deriving (Eq, Show)

newQ :: String -> Int -> Float -> Quality
newQ = Qua


capacityQ :: Quality -> Int -- cuantos túneles puede tolerar esta conexión
capacityQ (Qua canal int float) = int


delayQ :: Quality -> Float  -- la demora por unidad de distancia que sucede en las conexiones de este canal
delayQ (Qua canal int float) = float