{-# OPTIONS_GHC -Wall #-}
module Golf where

------------------- Exercise 1 -------------------

skips :: [a] -> [[a]]
skips = skips' 1
  where skips' n xs
          | n > length xs = []
          | otherwise     = list : skips' (n + 1) xs
            where list = [x | (x, i) <- zip xs [1..], i `mod` n == 0]


------------------- Exercise 2 -------------------

localMaxima :: [Integer] -> [Integer]
localMaxima xs = [y | [x,y,z] <- subs xs, y > x && y > z]
  where subs (a:b:c:rest) = [a,b,c] : subs (b:c:rest)
        subs  _           = []


------------------- Exercise 3 -------------------

histogram :: [Integer] -> String
histogram = mkHistogram . mkLines . histCount . map fromIntegral
  where histCount    = foldl count $ replicate 10 0
        count sums n = take n sums ++ [1 + sums !! n] ++ drop (n + 1) sums
        mkLines xs
          | (< 0) `all` xs = []
          | otherwise      = foldr mkStar "" xs : mkLines (subtract 1 <$> xs)
        mkStar i row
          | (i :: Integer) > 0 = '*' : row
          | otherwise          = ' ' : row
        mkHistogram  = (++ "==========\n0123456789") . unlines . reverse
