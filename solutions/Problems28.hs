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

module Problems28 where

insertAt :: x -> [x] -> Int -> [x]
insertAt x xs 1 = x:xs
insertAt _ [] _ = error "insertion index greater than array length"
insertAt x (hd:tl) n = hd:(insertAt x tl (n-1))

range :: (Enum x) => x -> x -> [x]
range x y = [x..y]
