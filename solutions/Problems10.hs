-- Copyright 2025 asdftowel

-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- You may obtain a copy of the License at

--     http://www.apache.org/licenses/LICENSE-2.0

-- Unless required by applicable law or agreed to in writing, software
-- distributed under the License is distributed on an "AS IS" BASIS,
-- WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
-- See the License for the specific language governing permissions and
-- limitations under the License.

module Problems10 where

myLast :: [x] -> x
myLast [] = error "cannot get last element from an empty list"
myLast [hd] = hd
myLast (_:tl) = myLast tl

-- or through guards:
-- myLast [] = error "cannot get last element from an empty list"
-- myLast (hd:tl)
--   | null tl = hd
--   | otherwise = myLast tl

myPLast :: [x] -> x
myPLast [] = error "cannot get second-last element from an empty list"
myPLast [x] = x
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

isPalindrome :: Eq x => [x] -> Bool
isPalindrome [] = False
isPalindrome xs = reverse xs == xs

data NestedList x = Elem x | List [NestedList x]

flatten :: NestedList x -> [x]
flatten x = innerFlat x []
  where
    innerFlat (List []) acc = acc
    innerFlat (Elem x) acc = x:acc
    innerFlat (List (hd:tl)) acc = innerFlat hd (innerFlat (List tl) acc)

compress :: Eq x => [x] -> [x]
compress (f:rest@(s:tl))
  | f == s    = compress rest
  | otherwise = f:(compress rest)
compress x = x

pack :: Eq x => [x] -> [[x]]
pack (hd:tl) = case li of
  (sub@(x:xs):rest) -> if hd == x
    then (x:sub):rest
    else [hd]:li
  [] -> [hd]:li
  where li = pack tl
pack [] = []

rle :: Eq x => [x] -> [(Int, x)]
rle (hd:tl) = case li of
  ((n, x):rest) -> if hd == x
    then (n + 1, x):rest
    else (1, hd):li
  [] -> (1, hd):li
  where li = rle tl
rle [] = []
