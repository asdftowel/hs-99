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
myLast (hd : tl) = foldl' (flip const) hd tl
myLast []        = error "empty list"

myButLast :: [x] -> x
myButLast (x : rest@(_ : ys))
  | null ys   = x
  | otherwise = myButLast rest
myButLast _ = error "not enough elements"

elementAt :: [x] -> Int -> x
elementAt (hd : tl) n
  | n == 1    = hd
  | otherwise = elementAt tl (n - 1)
elementAt [] _ = error "index too large"

myLength :: [x] -> Int
myLength = foldl' (const . (+ 1)) 0

myReverse :: [x] -> [x]
myReverse = foldl' (flip (:)) []

isPalindrome :: Eq x => [x] -> Bool
isPalindrome [] = False
isPalindrome li = reverse li == li

flatten :: NestedList x -> [x]
flatten li = fix (
  \f xs acc -> case xs of
    List []        -> acc
    Elem x         -> x : acc
    List (hd : tl) -> (f hd . f (List tl)) acc
                 ) li []

compress :: Eq x => [x] -> [x]
compress (f : rest@(s : _))
  | f == s    = compress rest
  | otherwise = f : compress rest
compress x = x

pack :: Eq x => [x] -> [[x]]
pack (hd : tl) = case li of
  (sub@(x : _) : rest) ->
    if hd == x
    then (x : sub) : rest
    else [hd] : li
  [] -> [hd] : li
  where li = pack tl
pack [] = []

rle :: Eq x => [x] -> [(Int, x)]
rle (hd : tl) = case li of
  ((n, x) : rest) ->
    if hd == x
    then (n + 1, x) : rest
    else (1, hd) : li
  [] -> (1, hd) : li
  where li = rle tl
rle [] = []
