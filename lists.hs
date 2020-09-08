select_evens :: [Int] -> [Int]
select_evens [] = []
select_evens (x:xs) = if x `mod` 2 == 0 then x:select_evens xs else select_evens xs


select_odds :: [Int] -> [Int]
select_odds [] = []
select_odds (x:xs) = if x `mod` 2 == 1 then x:select_odds xs else select_odds xs

member :: Int -> [Int] -> Bool
member n [] = False
member n (x:xs) | n == x = True | otherwise = member n xs


append :: [Int] -> [Int] -> [Int]
append [] ys = ys
append (x:xs) ys = x:append xs ys

revert :: [Int] -> [Int]
revert [] = []
revert (x:xs) = revert xs ++ [x]

zipp :: [Int] -> [Int] -> [Int]
zipp [] ys = ys
zipp (x:xs) ys = x:zipp ys xs

merge_sort :: [Int] -> [Int] -> [Int]
merge_sort [] ys = ys
merge_sort (x:xs) (y:ys) | x < y = x:merge_sort xs (y:ys) | otherwise = y:merge_sort (x:xs) ys
