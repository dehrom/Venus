factorial :: (Eq a, Num a, Ord a) => a -> a
factorial n | n < 0 = 0
            | n == 1 = 1
            | otherwise = n * factorial (n - 1)

sumMatch :: (Eq a, Num a, Ord a) => a -> a
sumMatch n | n < 0 = 0
           | n == 1 = 1
           | otherwise = n + sumMatch (n - 1)

contains :: (Eq a) => a -> [a] -> Bool
contains _ [] = False
contains e (x:xs) | e == x = True
                  | otherwise = contains e xs

makeUnique :: (Eq a) => [a] -> [a]
makeUnique [] = []
makeUnique (x:xs) =
  let isContains = contains x xs in
    (if isContains then makeUnique xs else x : makeUnique xs)

insert :: (Ord a) => a -> [a] -> [a]
insert x [] = [x]
insert x (y:ys) =
  if x <= y
  then x : y : ys
  else y : insert x ys

sort :: (Ord a) => [a] -> [a]
sort [] = []
sort xs = foldr insert [] xs

merge :: (a -> a -> Bool) -> [a] -> [a] -> [a]
merge _ [] l = l
merge _ l [] = l
merge cmp (x:xs) (y:ys) =
  if cmp x y
  then x : merge cmp xs (y : ys) else y : merge cmp (x : xs) ys

mergeSort :: (a -> a -> Bool) -> [a] -> [a]
mergeSort _ [] = []
mergeSort _ [x] = [x]
mergeSort cmp arr =
  let
    middleLength = div (length arr) 2
    left = take middleLength arr
    right = drop middleLength arr
  in
    merge cmp (mergeSort cmp left) (mergeSort cmp right)

isSorted :: Ord a => [a] -> Bool
isSorted [] = True
isSorted [x] = True
isSorted (x0:x1:arr) = (x0 < x1) && isSorted arr

apply :: (Num a, Eq a) => (a -> a) -> a -> a -> a
apply _ a 0 = a
apply fn a b = apply fn (fn a) (b - 1)
