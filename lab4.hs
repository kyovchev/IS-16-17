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
 print ("expected 4")
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
sumIntList :: [Integer] -> Integer
sumIntList _ = -1

-- Задача 2. Напишете фукнция, която намира сумата на елементите на списък от реални числа
sumDoubleList :: [Double] -> Double
sumDoubleList _ = -1

-- Задача 3. Напишете фукнция, която намира сумата на елементите на списък от числа
sumNumList :: Num t => [t] -> t
sumNumList _ = -1

-- Задача 4. Напишете фунция, която намира броя на елементите на списък
countList :: [t] -> Integer
countList _ = -1

-- Задача 5. Напишете предикат, който проверява дали даден елемент се среща в списък
memberOf :: Eq t => t -> [t] -> Bool
memberOf _ _ = False

-- Задача 6. Напишете функция, която премахва първото срещане на x в списъка xs
removeFirstOcc :: Eq t => t -> [t] -> [t]
removeFirstOcc _ _ = []

-- Задача 7. Напишете фунция, която връща елементът на позиция i в списъка xs
-- Заб.: Индексираме от 0.
elemAtIndex :: Integer -> [t] -> t
elemAtIndex _ (x:xs) = x

-- Задача 8. Напишете функция, която премахва всички срещания на x в списъка xs
removeAllOcc :: Eq t => t -> [t] -> [t]
removeAllOcc _ _ = []

-- Задача 9. Да се дефинира функция, която намира обединението на
-- множествата, представени чрез списъците xs и ys
unionSets :: Eq t => [t] -> [t] -> [t]
unionSets _ _ = []

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
