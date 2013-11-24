type Peg = String

type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []
hanoi 1 a b c = (a, c):hanoi 0 a b c
hanoi x a b c = hanoi (x - 1) a c b ++ [(a, c)] ++ hanoi (x - 1) b a c
