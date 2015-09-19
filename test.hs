length' :: (Integral b) => [a] -> b
length' [] = 0
length' (x:y) = 1+ length' y

sum' :: (Integral a) => [a] -> a
sum' [] = 0
sum' (x:xs) = x + sum' xs

fib x
  | x < 2 = 1
  | otherwise = fib (x - 1) + fib (x - 2)


sumlist :: Num a => [a] -> a
sumlist [] = 0
sumlist (h:t) = sumlist t + h

index :: Num i => i -> [a] -> a
index 0 (h:t) = "C"
index x [] = error
