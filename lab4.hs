{-
 Упражнение 4
 Списъци
 16.03.2017
-}

main :: IO()
main = do
 print (sumIntList [1,2,3])
 print ("expected 6")
 print ("---")
 print (sumDoubleList [1.5,2.5])
 print ("expected 4.0")
 print ("---")
 print (sumNumList [1,2,4])
 print ("expected 7")
 print ("---")
 print (countList [1,2,4])
 print ("expected 3")
 print ("---")
 print (memberOf 2 [1,2,4])
 print ("expected True")
 print ("---")
 print (f5 2 [1,2,4])
 print ("expected True")
 print ("---")
 print (f5 3 [1,2,4])
 print ("expected False")
 print ("---")
 print (removeFirstOcc 2 [1,2,4])
 print ("expected [1,4]")
 print ("---")
 print (elemAtIndex 2 [1,2,4])
 print ("expected 4")
 print ("---")
 print (removeAllOcc 2 [1,2,2,2,4])
 print ("expected [1,4]")
 print ("---")
 print (unionSets [1,2,3,4] [1,4,5])
 print ("expected [1,2,3,4,5]")
 print ("---")
 print (f9 [1,2,3,4] [1,4,5])
 print ("expected [1,2,3,4,5]")
 print ("---")
 print (f9 [] [1,4,5])
 print ("expected [1,4,5]")
 print ("---")
 print (f9 [1,2] [])
 print ("expected [1,2]")
 print ("---")
 print (f9 [1,2] [1,2])
 print ("expected [1,2]")
 print ("---")
 print (f9_2 [1,2,3,4] [1,4,5])
 print ("expected [1,2,3,4,5]")
 print ("---")
 print (f9_2 [] [1,4,5])
 print ("expected [1,4,5]")
 print ("---")
 print (f9_2 [1,2] [])
 print ("expected [1,2]")
 print ("---")
 print (f9_2 [1,2] [1,2])
 print ("expected [1,2]")
 print ("---")
 print (intersectSets [1,2,3,4] [1,4,5])
 print ("expected [1,4]")
 print ("---")
 print (mergeLists [1,2,3,4] [1,4,5])
 print ("expected [1,1,2,3,4,4,5]")
 print ("---")
 print (sortList [1,5,0,3,4])
 print ("expected [0,3,4,5]")
 print ("---")
 print (countOccur [1,2,3,2,1,4,1,2,3] [1,2])
 print ("expected 2")
 print ("---")

-- Задача 1. Напишете фукнция, която намира сумата на елементите на списък от цели числа
f1 :: [Integer] -> Integer
f1 [] = 0
f1 (x:xs) = x + f1 xs

-- Следващото решение е предложено от Георги Стойчев (https://github.com/GStoichev)
sumIntList :: [Integer] -> Integer
sumIntList list
 | list == [] = 0 
 | otherwise = (head list) + (sumIntList (tail list))

-- Задача 2. Напишете фукнция, която намира сумата на елементите на списък от реални числа
f2 :: [Double] -> Double
f2 [] = 0
f2 (x:xs) = x + f2 xs

-- Следващото решение е предложено от Георги Стойчев (https://github.com/GStoichev)
sumDoubleList :: [Double] -> Double
sumDoubleList list
 | list == [] = 0
 | otherwise = (head list) + (sumDoubleList (tail list))

-- Задача 3. Напишете фукнция, която намира сумата на елементите на списък от числа
f3 :: Num t => [t] -> t
f3 [] = 0
f3 (x:xs) = x + f3 xs

-- Следващото решение е предложено от Георги Стойчев (https://github.com/GStoichev)
sumNumList :: Num t => [t] -> t
sumNumList  list
 | (null list) = 0
 | otherwise = (head list) + (sumNumList (tail list))

-- Задача 4. Напишете фунция, която намира броя на елементите на списък
f4 :: [t] -> Integer
f4 [] = 0
f4 (x:xs) = 1 + f4 xs

-- Следващото решение е предложено от Георги Стойчев (https://github.com/GStoichev)
countList :: [t] -> Integer
countList list
 | (null list) = 0
 | otherwise = 1 + (countList (tail list))

-- Задача 5. Напишете предикат, който проверява дали даден елемент се среща в списък
f5 :: Eq t => t -> [t] -> Bool
f5 _ [] = False
f5 n (h:hs) = n == h || f5 n hs 

-- Следващото решение е предложено от Георги Стойчев (https://github.com/GStoichev)
memberOf :: Eq t => t -> [t] -> Bool
memberOf findMe list
 | (null list) = False
 | otherwise = if findMe == (head list) then True else (memberOf findMe (tail list))

-- Задача 6. Напишете функция, която премахва първото срещане на x в списъка xs
f6 :: Eq t => t -> [t] -> [t]
f6 _ [] = []
f6 n (h:hs) = if n == h then hs else h : (f6 n hs)

-- Следващото решение е предложено от Георги Стойчев (https://github.com/GStoichev)
removeFirstOcc :: Eq t => t -> [t] -> [t]
removeFirstOcc removeMe list = if (helper removeMe list []) == [] then list else  (helper removeMe list [])
 where
  helper :: Eq t => t -> [t] -> [t] -> [t] 
  helper removeMe list listForPrint 
   | (null list) = list
   | otherwise = 
    if removeMe == (head list)
    then listForPrint ++ (tail list)
    else helper removeMe (tail list) (listForPrint ++ ([(head list)])) 

-- Задача 7. Напишете фунция, която връща елементът на позиция i в списъка xs
-- Заб.: Индексираме от 0, 0 <= i < d, където d е дължината на списъка xs.
f7 :: Integer -> [t] -> t
f7 0 (x:_) = x
f7 i (_:xs) = f7 (i - 1) xs

-- Следващото решение е предложено от Георги Стойчев (https://github.com/GStoichev)
elemAtIndex :: Integer -> [t] -> t
elemAtIndex index list = helper index 0 list 
 where 
  helper :: Integer -> Integer -> [t] -> t
  helper index count list        
   | (null list) = error ("Length of list is less then i!")
   | otherwise = if index == count then (head list) else helper index (count + 1) (tail list)  

-- Задача 8. Напишете функция, която премахва всички срещания на x в списъка xs
f8 :: Eq t => t -> [t] -> [t]
f8 _ [] = []
f8 n (h:hs) = if n == h then f8 n hs else h : (f8 n hs)

-- Следващото решение е предложено от Георги Стойчев (https://github.com/GStoichev)
removeAllOcc :: Eq t => t -> [t] -> [t]
removeAllOcc removeMe list = 
 if list == (removeFirstOcc removeMe list)
 then list
 else removeAllOcc removeMe (removeFirstOcc removeMe list) 

-- Задача 9. Да се дефинира функция, която намира обединението на
-- множествата, представени чрез списъците xs и ys
-- Първи вариант
f9 :: Eq t => [t] -> [t] -> [t]
f9 [] ys = ys
f9 xs [] = xs
f9 (x:xs) ys = if elem x ys then f9 xs ys else x:(f9 xs ys)

-- Втори вариант, чрез list comprehension
f9_2 :: Eq t => [t] -> [t] -> [t]
f9_2 xs ys = [x | x <- xs, not (elem x ys)] ++ ys

-- Следващото решение е предложено от Георги Стойчев (https://github.com/GStoichev)
unionSets :: Eq t => [t] -> [t] -> [t]
unionSets list1 list2
 | (null list1) && (null list2) = error ("Both lists are empty!")
 | (null list1) = list2
 | (null list2) = list1
 | otherwise =  (someFunc list1 []) ++ (helper (someFunc list1 [])  (someFunc list2 []))
  where
   helper :: Eq t => [t] -> [t] -> [t]
   helper list1 list2 
    | (null list1) = list2
    | otherwise = helper (tail list1) (removeAllOcc (head list1) list2) 
   someFunc:: Eq t => [t] -> [t]-> [t]
   someFunc list listForReturn
    | (null list) = listForReturn 
    | otherwise = 
     if [head list] ++ (removeFirstOcc (head list) (tail list)) == list 
     then someFunc (tail list) (listForReturn ++ [head list]) 
     else someFunc (tail list) (listForReturn)

-- Задача 10. Да се дефинира процедура, която намира сечението на две
-- числови множества xs и ys, представени чрез списъци
intersectSets :: Eq t => [t] -> [t] -> [t]
intersectSets _ _ = []

-- Задача 11. Да се дефинира функция, която слива два сортирани списъка от реални числа
mergeLists :: Ord t => [t] -> [t] -> [t]
mergeLists _ _ = []

-- Задача 12. Да се дефинира функция, която сортира списък от реални числа
sortList :: Ord t => [t] -> [t]
sortList _ = []

-- Задача 13. Да се дефинира функция, която по зададени списъци xs и ys
-- връща като резултат броя на срещанията на списъка ys в списъка xs
countOccur :: Eq t => [t] -> [t] -> Integer
countOccur _ _ = 0
