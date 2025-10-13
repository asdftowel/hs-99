insertAt :: x -> [x] -> Int -> [x]
insertAt x xs 1 = x:xs
insertAt _ [] _ = error "insertion index greater than array length"
insertAt x (hd:tl) n = hd:(insertAt x tl (n-1))

range :: (Enum x) => x -> x -> [x]
range x y = [x..y]
