{-
 Упражнение 1
 Основни понятия
 23.02.2017
-}

main :: IO()
main = do
  print (factRec 5)
  print (factIter 5)
  print (fibRec 5)
  print (fibIter 5)

-- Задача 1. Пресмятане на факториел, чрез рекурсия
factRec :: Integer -> Integer
factRec n =
  if n <= 1 then 1 else n * factRec (n - 1)

-- Задача 2. Пресмятане на факториел, чрез итерация
factIter :: Integer -> Integer
factIter n = helper 2 1
  where
    helper :: Integer -> Integer -> Integer
    helper i res =
      if i > n then res else helper (i + 1) (res * i)

-- Задача 3. Рекурсивно пресмятане на n-тото число на фибоначи
-- Заб.: Числата в редицата 1, 1, 2, 3, 5, 8, ... са индексирани са от 0
fibRec :: Integer -> Integer
fibRec 0 = 1
fibRec 1 = 1
fibRec n = fibRec (n - 2) + fibRec (n - 1)

-- Задача 4. Итеративно пресмятане на n-тото число на фибоначи
-- Заб.: Числата в редицата 1, 1, 2, 3, 5, 8, ... са индексирани са от 0
fibIter :: Integer -> Integer
fibIter n = helper 0 1 0
  where
    helper :: Integer -> Integer -> Integer -> Integer
    helper prev curr currIndex =
      if currIndex == n then curr else helper curr (prev + curr) (currIndex + 1)
