data Stick = Vacio | Stack Stick Int deriving ( Eq )

pop :: Stick -> Stick
pop (Stack s n) = s

push :: Stick -> Int -> Stick
push Vacio n = Stack Vacio n
push s@(Stack _ d) n | d > n = Stack s n
-- push s n | top s > n = Stack s n

top :: Stick -> Int
top (Stack _ d) = d

data Hanoi = Hanoi Stick Stick Stick deriving ( Eq )

instance Show Hanoi
        where show (Hanoi i c d) = " I: " ++ show i ++ 
                "\n C: " ++ show c ++ 
                "\n D: " ++ show d
instance Show Stick
        where show s = "| " ++ printS s

printS Vacio = ""
printS (Stack s d ) = printS s ++ " " ++ show d


moveIC :: Hanoi -> Hanoi
moveIC (Hanoi i c d) = Hanoi (pop i) (push c (top i)) d

stickWith :: [Int] -> Stick
stickWith = foldr (\each fold -> push fold each) Vacio

initWith :: [Int] -> [Int] -> [Int] -> Hanoi
initWith i c d = Hanoi (stickWith i) (stickWith c) (stickWith d)

hanoi = initWith [] [1,3] [2]

t = [ pop ( Stack Vacio 2) == Vacio,
      push Vacio 3 == Stack Vacio 3,
      top (push Vacio 3) == 3,
      stickWith [ 2, 3 ] == Stack (Stack Vacio 3) 2,
      True ]