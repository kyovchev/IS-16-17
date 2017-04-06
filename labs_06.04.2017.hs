import Data.List
main :: IO()
main = do
       print("Task 1")
       print("Entered data : [1,5,7] and [3,4,9] output : ")
       print(merge [1,5,7] [3,4,9])
       print("Task 2")
       print("Entered data : \"Can you split these words?\" and ' ' ")
       print(splitWords "Can you split these words?" ' ')
       print("Task 3")
       print("a)")
       print("Entered data : \"abc\" and 2")
       print(rotate "abc" 2)
       print("b)")
       print("Entered data : \"abc\"")
       print(rotations "abc")
       print("c)")
       print("Entered data : \"BANANA\"")
       print(bwt "BANANA")
--------------------------------TASK1----------------------------------
merge :: [Int] -> [Int] -> [Int]
merge firstList secondList = helper firstList secondList [] 
    where
       helper :: [Int] -> [Int] -> [Int] -> [Int]
       helper [] secondList fillME= fillME++secondList
       helper firstList [] fillME= fillME++firstList
       helper (x:xs) (y:ys) fillME = if (min x y) == x  
                                     then helper xs (y:ys) (fillME++[x]) 
                                     else helper (x:xs) ys (fillME++[y])
-----------------------------------------------------------------------

--------------------------------TASK2----------------------------------
splitWords:: String -> Char -> [String]
splitWords str c = reverse (helper str c "" [])
    where
       helper :: String -> Char -> String -> [String] -> [String]
       helper [] c word fillME = word:fillME
       helper (x:xs) c word fillME = if x == c 
                                     then helper xs c "" (word:fillME) 
                                     else helper xs c (word++[x]) fillME 
-----------------------------------------------------------------------

--------------------------------TASK3----------------------------------
--a)
rotate :: String -> Int -> String
rotate str rotates = helper str 0 rotates 
    where
        helper :: String -> Int -> Int -> String
        helper [] _ _= []
        helper (x:xs) count max 
         | count==max = (x:xs)
         | otherwise = helper (xs++[x]) (count+1) max
--b)
rotations :: String -> [String]
rotations str = reverse (helper str 0 (length str) [])
    where 
        helper :: String -> Int -> Int -> [String] ->[String]
        helper [] count length  fillME = []
        helper str count length fillME 
         | count == length = fillME
         | otherwise = helper (rotate str 1) (count+1) length (str:fillME)
--c)
bwt :: String -> String
bwt str = takeLast (sort (rotations str)) ""
    where
     takeLast :: [String] -> String -> String
     takeLast [] fillME = fillME
     takeLast list fillME = map last list
-----------------------------------------------------------------------
