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

module Problems20 where

import           Problems10 (rle)

data ElementGroup x = Single x | Multiple Int x deriving Show

{-
Some replacements for Prelude functions:
myReverse from Problems10
list and tuple indexing can be trivially implemented
through pattern matching
myReplicate :: Int -> x -> [x]
myReplicate 0 _ = []
myReplicate n x = x:(myReplicate (n-1) x)
-}

rle' :: Eq x => [x] -> [ElementGroup x]
rle' xs = [
  if fst x == 1
  then (Single . snd) x
  else Multiple (fst x) (snd x)
  | x <- rle xs
          ]

decompress :: [ElementGroup x] -> [x]
decompress []                   = []
decompress ((Single hd):tl)     = hd:(decompress tl)
decompress ((Multiple n hd):tl) = (replicate n hd) ++ (decompress tl)

typedRLE :: Eq x => [x] -> [ElementGroup x]
typedRLE (hd:tl) = case li of
  ((Multiple n x):rest) -> if hd == x
    then (Multiple (n + 1) x):rest
    else (Single hd):li
  ((Single x):rest) -> if hd == x
    then (Multiple 2 x):rest
    else (Single hd):li
  [] -> (Single hd):li
  where li = typedRLE tl
typedRLE [] = []

dupli :: [x] -> [x]
dupli []      = []
dupli (hd:tl) = hd:hd:(dupli tl)

repli :: Int -> [x] -> [x]
repli n xs = subRep n xs
  where
    subRep :: Int -> [x] -> [x]
    subRep _ [] = []
    subRep i li@(hd:tl)
      | i == 1    = hd:(subRep n tl)
      | otherwise = hd:(subRep (i-1) li)

dropEvery :: Int -> [x] -> [x]
dropEvery n x = subDrop n x
  where
    subDrop :: Int -> [x] -> [x]
    subDrop _ [] = []
    subDrop i (hd:tl)
      | i == 1    = subDrop n tl
      | otherwise = hd:(subDrop (i - 1) tl)

split :: Int -> [x] -> ([x], [x])
split _ []      = error "index too large"
split 1 (hd:tl) = ([hd], tl)
split x (hd:tl) = (hd:(fst tup), snd tup)
  where tup = split (x-1) tl

slice :: [x] -> Int -> Int -> [x]
slice xs m n = take (n - l) (drop l xs)
  where l = m - 1

rotate :: [x] -> Int -> [x]
rotate [] _ = []
rotate xs 0 = xs
rotate xs n = snd parts ++ fst parts
  where parts = splitAt (mod n (length xs)) xs

removeAt :: Int -> [x] -> (x, [x])
removeAt _ []      = error "index too large"
removeAt 1 (hd:tl) = (hd, tl)
removeAt x (hd:tl) = (fst tup, hd:(snd tup))
  where tup = removeAt (x-1) tl
