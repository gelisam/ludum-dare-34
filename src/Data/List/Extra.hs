module Data.List.Extra where


-- the merge step of merge sort.
-- the two input lists must already by sorted.
-- HACK: sorted from highest to lowest, because coordinates decrease as we go up.
merge :: Ord a => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys) | x >= y    = x : merge xs (y:ys)
                    | otherwise = y : merge (x:xs) ys

mergeOn :: (Show a, Show b, Ord b) => (a -> b) -> [a] -> [a] -> [a]
mergeOn _ [] ys = ys
mergeOn _ xs [] = xs
mergeOn f (x:xs) (y:ys) | f x >= f y = x : mergeOn f xs (y:ys)
                        | otherwise  = y : mergeOn f (x:xs) ys
