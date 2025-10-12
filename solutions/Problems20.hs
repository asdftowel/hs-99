import Problems10 (rle)

data ElementGroup x = Single x | Multiple Int x deriving Show

-- Some replacements for Prelude functions:
-- myReverse from Problems10
-- list and tuple indexing can be trivially implemented
-- through pattern matching
-- replicate' :: Int -> x -> [x]
-- replicate' 0 _ = []
-- replicate' n x = x:(replicate' (n-1) x)

rle' :: (Eq x) => [x] -> [ElementGroup x]
rle' xs = [
  if fst x == 1
  then (Single . snd) x
  else Multiple (fst x) (snd x)
  | x <- rle xs
          ]

decompress :: [ElementGroup x] -> [x]
decompress [] = []
decompress ((Single hd):tl) = hd:(decompress tl)
decompress ((Multiple n hd):tl) = (replicate n hd) ++ (decompress tl)

typedRLE :: (Eq x) => [x] -> [ElementGroup x]
typedRLE [] = []
typedRLE [x] = [(Single x)]
typedRLE (hd:tl) = case head li of
  (Single x) -> if hd == x
    then (Multiple 2 hd):(tail li)
    else (Single hd):li
  (Multiple n x) -> if hd == x
    then (Multiple (n+1) hd):(tail li)
    else (Single hd):li
  where li = typedRLE tl

dupli :: [x] -> [x]
dupli [] = []
dupli (hd:tl) = hd:hd:(dupli tl)

repli :: Int -> [x] -> [x]
repli n xs = subRep n xs
  where
    subRep :: Int -> [x] -> [x]
    subRep _ [] = []
    subRep i all@(hd:tl)
      | i == 1 = hd:(subRep n tl)
      | otherwise = hd:(subRep (i-1) all)

dropEvery :: Int -> [x] -> [x]
dropEvery n x = subDrop n x
  where
    subDrop :: Int -> [x] -> [x]
    subDrop _ [] = []
    subDrop i (hd:tl)
      | i == 1    = subDrop n tl
      | otherwise = hd:(subDrop (i - 1) tl)

split :: Int -> [x] -> ([x], [x])
split _ [] = error "index too large"
split 1 (hd:tl) = ([hd], tl)
split x (hd:tl) = (hd:(fst tup), snd tup)
  where tup = split (x-1) tl

slice :: [x] -> Int -> Int -> [x]
slice x i k
  | i < 0 || k > length x = error "slice larger than array"
  | k < i                 = error "start is greater than end"
  | otherwise             = subSlice x 1
  where
    subSlice [] _ = []
    subSlice (hd:tl) n
      | n < i = subSlice tl (n + 1)
      | n <= k = hd:(subSlice tl (n + 1))
      | otherwise = []

-- this solution is naive, but it has no arbitrary
-- constraints, so n can be greater than the
-- list's length.
rotate :: Int -> [x] -> [x]
rotate _ [] = []
rotate n all@(hd:tl)
  | n == 0    = all
  | n  > 0    = rotate (n - 1) (tl ++ [hd])
  | otherwise = rotate (n + 1) ((last all):(init all))

removeAt :: Int -> [x] -> (x, [x])
removeAt _ [] = error "index too large"
removeAt 1 (hd:tl) = (hd, tl)
removeAt x (hd:tl) = (fst tup, hd:(snd tup))
  where tup = removeAt (x-1) tl

