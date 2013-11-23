
validate :: Integer -> Bool
validate x = digitSum `mod` 10 == 0  
    where digitSum = sumDigits $ doubleEveryOther $ toDigitsRev x


toDigits :: Integer -> [Integer]
toDigits = reverse . toDigitsRev


toDigitsRev :: Integer -> [Integer]
toDigitsRev x
    | x <= 0 = []
    | otherwise = (x `mod` 10):(toDigitsRev $ x `div` 10)


doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther [x] = [x]
doubleEveryOther (x:y:xs) = x:y+y:doubleEveryOther xs


sumDigits :: [Integer] -> Integer
sumDigits xs = sum $ map sumDigit xs


sumDigit :: Integer -> Integer
sumDigit x
    | x < 10 = x
    | otherwise = sum $ toDigits x
