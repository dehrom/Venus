factorial :: (Eq a, Num a, Ord a) => a -> a
factorial n | n < 0 = 0
            | n == 1 = 1
            | otherwise = n * factorial (n - 1)
            
fibonacci :: Integer -> Integer
fibonacci n | n == 0 = 0
            | n == 1 = 1
            | n > 1 = fibonacci (n - 1) + fibonacci (n - 2)
            | n < 0 = fibonacci (n + 2) - fibonacci (n + 1)

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

myFilter :: (Eq a) => (a -> Bool) -> [a] -> [a]
myFilter _ [] = []
myFilter fn (x:xs) = 
  if fn x 
    then x : myFilter fn xs
    else myFilter fn xs

myAll :: (Eq a) => (a -> Bool) -> [a] -> Bool
myAll _ [] = True
myAll fn (x:xs) = fn x && myAll fn xs

mapl :: (a -> b) -> [[a]] -> [[b]]
mapl _ [] = []
mapl fn (x:xs) = map fn x : mapl fn xs

fn0 :: (Fractional a) => [a] -> [a]
fn0 = reverse . map (/ 15)

smallestPositive :: (Eq a, Num a, Ord a) => [a] -> Maybe a
smallestPositive [] = Nothing
smallestPositive xs = 
  case 
    sort $ myFilter (> 0) xs
  of
    [] -> Nothing
    xs -> Just $ head xs

mapMaybe :: (a -> Maybe b) -> b -> [a] -> [b]
mapMaybe _  _ [] = []
mapMaybe fn d xs = map (\x -> 
    case fn x of
      Nothing -> d
      Just el -> el
  ) xs

remove :: (Eq a) => a -> [(a, b)] -> [(a, b)]
remove _ [] = []
remove i ((k, v):xs) = 
  if k == i
  then xs
  else (k, v) : remove k xs

addIfNotExist :: (Eq k) 
              => (k, v)
              -> [(k, v)]
              -> Maybe [(k, v)]
addIfNotExist (k, v) xs =
  case lookup k xs of
    Nothing -> Just $ xs ++ [(k, v)]
    Just _ -> Nothing

myZip :: [a] 
      -> [b] 
      -> [(a, b)]
myZip _ [] = []
myZip [] _ = []
myZip (k:ks) (v:vs) = (k, v) : myZip ks vs 

myUnzip :: [(a, b)]
        -> ([a], [b])
myUnzip [] = ([], [])
myUnzip ((k, v): xs) = (k: ks, v: vs) where (ks, vs) = myUnzip xs

union :: [(a, b)] 
      -> [(a, b)] 
      -> [(a, b)]
union [] [] = []
union (x:xs) (x':xs') = x : x' : union xs xs'
