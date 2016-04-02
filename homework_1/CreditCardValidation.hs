module CreditCardValidation(validate) where

validate :: Int -> Bool
validate num = let calculate = sumDigits . doubleEveryOther . toDigitsRev
               in  calculate num `rem` 10 == 0


doubleEveryOther :: [Int] -> [Int]
doubleEveryOther (x1:x2:xs) = x1 : (2 * x2) : doubleEveryOther xs
doubleEveryOther xs = xs

toDigitsRev :: Int -> [Int]
toDigitsRev i
  | i <= 0    = []
  | otherwise = i `mod` 10 : toDigitsRev (i `div` 10)

sumDigits :: [Int] -> Int
sumDigits nums = let digits = concat $ toDigitsRev <$> nums
                 in  sum digits
