-- http://behalek.cs.vsb.cz/

-- princip castence vyhodnocenych fci

-- let maxPartial = max 2
-- maxPartial 2

-- compareWithHundred:: int -> int -> bool
compareWithHundred :: (Num a, Ord a) => a -> Ordering
compareWithHundred x = compare 100 x
-- compareWithHundred 100

-- str 67

-- fce  jako operator ponizuje prioritu

-- predifinovavani priorit a asociativity vyhodnoce operaci pomoci napr. `inflixl` -- str. 69


-- nekonecne seznamy
-- list comprehention - generace seznamu, ktery splnuje nejake podminky

-- pr.: [x*2 | x<-[1..5]]
