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
sumIntList list
 | list == [] = 0 
 | otherwise = (head list) + (sumIntList (tail list))

-- Задача 2. Напишете фукнция, която намира сумата на елементите на списък от реални числа
sumDoubleList :: [Double]->Double
sumDoubleList list
 | list == [] = 0
 | otherwise = (head list) + (sumDoubleList (tail list))

-- Задача 3. Напишете фукнция, която намира сумата на елементите на списък от числа
sumNumList :: Num t => [t] -> t
sumNumList  list
 | (null list) = 0
 | otherwise = (head list) + (sumNumList (tail list))

-- Задача 4. Напишете фунция, която намира броя на елементите на списък
countList :: Eq t => [t] -> Integer
countList list
 | (null list) = 0
 | otherwise = 1 + (countList (tail list))

-- Задача 5. Напишете предикат, който проверява дали даден елемент се среща в списък
memberOf :: Eq t => t -> [t] -> Bool
memberOf findMe list
 | (null list) = False
 | otherwise = if findMe == (head list) then True else (memberOf findMe (tail list))

-- Задача 6. Напишете функция, която премахва първото срещане на x в списъка xs
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
-- Заб.: Индексираме от 0.
elemAtIndex :: Eq t => Integer -> [t] -> t
elemAtIndex index list = helper index 0 list 
 where 
  helper :: Eq t => Integer -> Integer -> [t] -> t
  helper index count list        
   | (null list) = error ("Length of list is less then i!")
   | otherwise = if index == count then (head list) else helper index (count + 1) (tail list)  

-- Задача 8. Напишете функция, която премахва всички срещания на x в списъка xs
removeAllOcc :: Eq t => t -> [t] -> [t]
removeAllOcc removeMe list = 
 if list == (removeFirstOcc removeMe list)
 then list
 else removeAllOcc removeMe (removeFirstOcc removeMe list) 

-- Задача 9. Да се дефинира функция, която намира обединението на
-- множествата, представени чрез списъците xs и ys
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
