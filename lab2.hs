{-
 Упражнение 2
 Рекурсия
 02.03.2017
-}

main :: IO()
main = do
 print (sumOfDigits 342)
 print ("expected 9")
 print ("---")
 print (intervalSum 7 11)
 print ("expected 45")
 print ("---")
 print (sumOfDivisors 5)
 print ("expected 6")
 print ("---")
 print (isPrime 13)
 print ("expected True")
 print ("---")
 print (reverseDigits 1374)
 print ("expected 4731")
 print ("---")
 print (countOccurences 0 1010102)
 print ("expected 3")
 print ("---")
 print (nonDecreasingNumber 1111233459)
 print ("expected True")
 print ("---")
 print (isPerfect 6)
 print ("expected True")
 print ("---")
 print (sumOfPrimeDivisors 8)
 print ("expected 2")
 print ("---")

-- Задача 1. Да се намери сумата от цифрите на дадено число
sumOfDigits :: Integer -> Integer
sumOfDigits 0 = 0
sumOfDigits n = n `mod` 10 + sumOfDigits (n `div` 10)

-- Задача 2. Да се намери сумата на целите числа в целочислен интервал
intervalSum :: Integer -> Integer -> Integer
intervalSum a b =
 if a > b
  then 0
  else a + intervalSum (a + 1) b

-- Задача 3. Да се намери сборът на всички делители на дадено число
-- Заб.: Вкл. 1 и самото число
sumOfDivisors :: Integer -> Integer
sumOfDivisors n = helper 1
 where
  helper :: Integer -> Integer
  helper d
   |d > n        = 0
   |mod n d == 0 = d + helper (d + 1)
   |otherwise    = helper (d + 1)

-- Задача 4. Да се провери дали дадено число е просто
-- Заб.: 1 не е нито просто, нито съставно
isPrime :: Integer -> Bool
isPrime 1 = False
isPrime n = helper 2
 where
  helper :: Integer -> Bool
  helper d
   |d == n       = True
   |mod n d == 0 = False
   |otherwise    = helper (d + 1)

-- Задача 5. Да се обърнат цифрите на дадено число
reverseDigits :: Integer -> Integer
reverseDigits x = helper 0 x
 where 
  helper :: Integer -> Integer -> Integer 
  helper y x 
   |x < 10 && x >= 0 = y + x
   |x < 0 = -1 
   |otherwise = helper ((y + (mod x 10)) * 10) (div x 10)

-- Задача 6. Да се напише функция, намираща броя на срещанията на дадена цифра в записа на число
countOccurences :: Integer -> Integer -> Integer
countOccurences digit num = helper num digit 1
 where
  helper :: Integer -> Integer -> Integer-> Integer 
  helper x y z 
   |x == 0 = x
   |otherwise = if (mod x 10) == y then z + helper (div x 10) y z else helper (div x 10) y z

-- Задача 7. Да се напише предикат, който връща истина, ако цифрите на дадено число са в нарастващ ред от първата към последната
nonDecreasingNumber :: Integer -> Bool
nonDecreasingNumber x
 |x < 10 = True
 |otherwise = if (helper x) == (-1)  then False else True 
  where
   helper :: Integer -> Integer
   helper x
    | x < 10 = 1 
    | otherwise = if (mod x 10) >= (mod (div x 10) 10) then helper (div x 10) else (-1)

-- Задача 8. Да се напише предикат, който връща дали едно число е съвършено, т.е. равно на сумата от делите си
-- Пример: 6 = 1 + 2 + 3
isPerfect :: Integer -> Bool
isPerfect n = sumOfDivisors n == 2 * n

-- Задача 9. Да се дефинира функция, която намира сумата на всички прости делители на едно число
sumOfPrimeDivisors :: Integer -> Integer
sumOfPrimeDivisors n = helper 1
 where
  helper :: Integer -> Integer
  helper d
   |d > n                     = 0
   |mod n d == 0 && isPrime d = d + helper (d + 1)
   |otherwise                 = helper (d + 1)
