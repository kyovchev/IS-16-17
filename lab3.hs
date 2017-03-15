{-
 Упражнение 3
 Отново рекурсия
 09.03.2017

 Отново разгледахме задачите от упражнение 2
-}

main :: IO()
main = do
  print (sumOfSeries 3 2)
  print ("expected 13")
  print ("---")
  print (sumOfSeriesFast 3 3)
  print ("expected 40")
  print ("---")
  print (sumOfSeriesIter 3 3)
  print ("expected 40")
  print ("---")

-- Задача 1. По зададени x и n, да се изчисли сумата: 1 + x + x^2 + x^3 + ... + x^n
sumOfSeries :: Double -> Integer -> Double
sumOfSeries x n 
 | n == -1 = 0 
 | otherwise = x^n + sumOfSeries x (n-1)

-- Задача 2. Да се реши задача 1, чрез използване на не повече от n умножения
sumOfSeriesFast :: Double -> Integer -> Double
sumOfSeriesFast x n 
 | n == -1 = 0
 | otherwise = 1 + x * sumOfSeriesFast x (n-1)

-- Задача 3. Да се реши задача 1, чрез итерация
sumOfSeriesIter :: Double -> Integer -> Double
sumOfSeriesIter x n = helper 0 1 0
 where
  helper :: Integer -> Double -> Double -> Double
  helper i ai sum =
   if i > n then sum else helper (i + 1) (ai * x) (sum + ai)