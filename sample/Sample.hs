import System.Environment
import Text.Read


main = getArgs >>= parse

parse :: [String] -> IO ()
parse [] = putStrLn "Please specify a number."
parse [x] = putStrLn $ compute (readMaybe x)
parse xs = putStrLn "Please specify one number."

compute :: Maybe Integer -> String
compute Nothing = "Please specify a number."
compute (Just x) = show $ bitReverseInt x

bitReverseInt = bitsToNum . numToBitAndReverse

numToBitAndReverse :: Integer -> [Integer]
numToBitAndReverse 0 = []
numToBitAndReverse x = bit:numToBitAndReverse  remainder
    where bit = x `rem` 2
          remainder = x `quot` 2

bitsToNum :: [Integer] -> Integer
bitsToNum [] = 0
bitsToNum (x:xs) = x * power + bitsToNum xs
    where power = toInteger $ 2 ^ (length xs)

