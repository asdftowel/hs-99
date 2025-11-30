{-
Copyright 2025 asdftowel

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
-}

module Problems10 where

import           Data.Function (fix)
import           Data.List     (foldl')

data NestedList x = Elem x | List [NestedList x]

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
elementAt [] _      = error "index too large"
elementAt (hd:tl) 1 = hd
elementAt (hd:tl) n = elementAt tl (n - 1)

myLength :: [x] -> Int
myLength []     = 0
myLength (_:tl) = 1 + myLength tl

-- Simplest example of list folding
myReverse :: [x] -> [x]
myReverse x = foldl' (flip (:)) [] x

isPalindrome :: Eq x => [x] -> Bool
isPalindrome [] = False
isPalindrome xs = reverse xs == xs

flatten :: NestedList x -> [x]
flatten li = fix (
  \f xs acc -> case xs of
    List []        -> acc
    Elem x         -> x : acc
    List (hd : tl) -> (f hd . f (List tl)) acc
                 ) li []

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
