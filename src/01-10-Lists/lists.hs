module List1 where

safeLast :: [a] -> Maybe a
safeLast [] = Nothing
safeLast (x:xs)
  | length xs == 0 = Just $ x
  | otherwise = Just $ head $ reverse xs

secondLast :: [a] -> Maybe a
secondLast [] = Nothing
secondLast (x: xs)
  | length xs == 0 = Nothing
  | length xs == 1 = Just x
  | otherwise = Just $ (reverse xs) !! 1


elementAt :: Int -> [a] -> Maybe a
elementAt _ [] = Nothing
elementAt pos x
  | length x < pos = Nothing
  | otherwise = Just $ x !! (pos - 1)


myLength :: [a] -> Int
myLength [] = 0
myLength (x:xs) = 1 + myLength xs

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]

isPalindrome :: Eq a => [a] -> Bool
isPalindrome x = reverse x == x

