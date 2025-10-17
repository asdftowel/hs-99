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
split _ [] = error "index too large"
split 1 (hd:tl) = ([hd], tl)
split x (hd:tl) = (hd:(fst tup), snd tup)
  where tup = split (x-1) tl

slice :: [x] -> Int -> Int -> [x]
slice xs m n = take (n - l) (drop l xs)
  where l = m - 1

-- this solution is naive, but it has no arbitrary
-- constraints, so n can be greater than the
-- list's length.
rotate :: [x] -> Int -> [x]
rotate [] _ = []
rotate xs 0 = xs
rotate xs n = snd parts ++ fst parts
  where parts = splitAt (mod n (length xs)) xs

removeAt :: Int -> [x] -> (x, [x])
removeAt _ [] = error "index too large"
removeAt 1 (hd:tl) = (hd, tl)
removeAt x (hd:tl) = (fst tup, hd:(snd tup))
  where tup = removeAt (x-1) tl

