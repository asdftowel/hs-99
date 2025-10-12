module Problems10 where

myLast :: [x] -> x
myLast []     = error "cannot get last element from an empty list"
myLast [hd]   = hd
myLast (_:tl) = myLast tl

-- or through guards:
-- myLast [] = error "cannot get last element from an empty list"
-- myLast (hd:tl)
--   | null tl = hd
--   | otherwise = myLast tl

myPLast :: [x] -> x
myPLast []     = error "cannot get second-last element from an empty list"
myPLast [x]    = x
myPLast [x, _] = x
myPLast (_:tl) = myPLast tl

elementAt :: [x] -> Int -> x
elementAt [] _ = error "index too large"
elementAt (hd:tl) n
  | n  < 0    = undefined
  | n == 0    = hd
  | otherwise = elementAt tl (n - 1)

myLength :: [x] -> Int
myLength [] = 0
myLength (_:tl) = 1 + myLength tl

myReverse :: [x] -> [x]
myReverse x = innerRev x []
  where
    innerRev [] acc = acc
    innerRev (hd:tl) acc = innerRev tl (hd:acc)

isPalindrome :: (Eq x) => [x] -> Bool
isPalindrome [] = False
isPalindrome xs =
  let l = div (length xs) 2
  in (take l xs) == (take l $ reverse xs)

data NestedList x = Elem x | List [NestedList x]

flatten :: NestedList x -> [x]
flatten x = innerFlat x []
  where
    innerFlat (List []) acc = acc
    innerFlat (Elem x) acc = x:acc
    innerFlat (List (hd:tl)) acc = innerFlat hd (innerFlat (List tl) acc)

compress :: (Eq x) => [x] -> [x]
compress (f:rest@(s:tl))
  | f == s = compress rest
  | otherwise = f : (compress rest)
compress x = x
-- or:
-- compress [] = []
-- compress [x] = [x]

pack :: (Eq x) => [x] -> [[x]]
pack [] = []
pack [x] = [[x]]
pack (hd:tl)
  | hd == ((head . head) li) = (hd:head li):tail li
  | otherwise = [hd]:li
  where li = pack tl

rle :: (Eq x) => [x] -> [(Int, x)]
rle [] = []
rle [x] = [(1, x)]
rle (hd:tl)
  | hd == ((snd . head) li) = (((fst . head) li) + 1, hd):tail li
  | otherwise = (1, hd):li
  where li = rle tl
