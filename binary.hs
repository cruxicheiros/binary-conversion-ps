module Binary where

data BinaryDigit = Zero | One deriving (Show, Eq, Ord)
type BinaryString = [BinaryDigit]

flipBit :: BinaryDigit -> BinaryDigit
flipBit Zero = One
flipBit One = Zero

padZeros :: BinaryString -> Int -> BinaryString
padZeros bs 0 = bs
padZeros bs n = (replicate n Zero) ++ bs

chrToBinDigit :: Char -> BinaryDigit
chrToBinDigit c
    | c == '1' = One
    | c == '0' = Zero
    | otherwise = error "Only valid characters for conversion are 1 and 0"
    
strToBin :: String -> BinaryString
strToBin [] = []
strToBin cs = map chrToBinDigit cs

binToStr :: BinaryString -> String
binToStr [] = ""
binToStr (d:ds)
    | d == One = '1' : binToStr ds 
    | d == Zero = '0' : binToStr ds
    
binDigitToInt :: BinaryDigit -> Int
binDigitToInt One = 1
binDigitToInt Zero = 0

binDigitToBool :: BinaryDigit -> Bool
binDigitToBool One = True
binDigitToBool Zero = False

singleDigitToBinDigit :: Int -> BinaryDigit
singleDigitToBinDigit 1 = One
singleDigitToBinDigit 0 = Zero
singleDigitToBinDigit _ = error "Only ints 0 and 1 are valid inputs"

-- Converts ints greater than zero to a list of 1s and 0s ordered RTL
intToDigitListWithoutZeroRTL :: Int -> [Int]
intToDigitListWithoutZeroRTL 0 = []
intToDigitListWithoutZeroRTL n = (n `mod` 2) : intToDigitListWithoutZeroRTL (n `div` 2)

-- Converts ints including zero to a list of 1s and 0s ordered RTL
intToDigitListRTL :: Int -> [Int]
intToDigitListRTL 0 = [0]
intToDigitListRTL n = intToDigitListWithoutZeroRTL n

-- Converts ints including zero to a list of 1s and 0s ordered LTR (the standard ordering)
intToDigitList :: Int -> [Int]
intToDigitList n = reverse (intToDigitListRTL n)

-- Handles standard binary representation. 
-- Satisfies Worksheet 1 Problem 7
intToBin :: Int -> BinaryString
intToBin n
    | n < 0 = error "Only positive integers can be converted to standard binary representation"
    | otherwise = [singleDigitToBinDigit x | x <- (intToDigitList n)]

-- Handles standard binary representation
-- Satisfies Worksheet 1 Problem 8 
binToInt :: BinaryString -> Int
binToInt [] = error "Can't convert an empty binary string"
binToInt [Zero] = 0
binToInt [One] = 1
binToInt (One:ds) = (2 ^ exp) + binToInt ds where exp = length ds
binToInt (Zero:ds) = binToInt ds

-- Converts standard binary to float, leftmost bit equivalent to 2^exp.
binToFloat :: BinaryString -> Float -> Float
binToFloat [] exp = error "Can't convert an empty binary string"
binToFloat [Zero] exp = 0
binToFloat [One] exp = (2 ** exp)
binToFloat (One:ds) exp = (2 ** exp) + binToFloat ds (exp - 1)
binToFloat (Zero:ds) exp = binToFloat ds (exp - 1)

signMagToInt :: BinaryString -> Int
signMagToInt [] = error "Can't convert an empty binary string"
signMagToInt [b] = error "Sign Magnitude Binary string consisting of only a sign digit is not valid"
signMagToInt (Zero:ds) = binToInt ds
signMagToInt (One:ds) = negate (binToInt ds)

twosToInt :: BinaryString -> Int
twosToInt [] = error "Can't convert an empty binary string"
twosToInt [b] = error "Two's Complement Binary string consisting of only a sign digit is not valid"
twosToInt (Zero:ds) = binToInt ds
twosToInt (One:ds) = (binToInt ds) - (2 ^ exp) where exp = (length ds) + 1

onesToInt :: BinaryString -> Int
onesToInt [] = error "Can't convert an empty binary string"
onesToInt [b] = error "One's Complement Binary string consisting of only a sign digit is not valid"
onesToInt (Zero:ds) = binToInt ds
onesToInt (One:ds) = negate $ binToInt (map flipBit ds)

excessToInt :: BinaryString -> Int -> Int
excessToInt [] bias = error "Can't convert an empty binary string"
excessToInt ds bias
    | bias < 0 = error "Bias must be positive"
    | bias == 0 = binToInt ds
    | otherwise = (binToInt ds) - bias

-- Convert signed magnitude to ones complement without first converting to int
-- Satisfies Worksheet 1 Problem 9
signMagToOnes :: BinaryString -> BinaryString
signMagToOnes [] = error "Can't convert an empty binary string"
signMagToOnes (Zero:ds) = (Zero : ds)
signMagToOnes (One:ds) = map flipBit (Zero : ds)


-- Convert One's Complement to signed magnitude without first converting to int
-- Satisfies Worksheet 1 Problem 10
onesToSignMag :: BinaryString -> BinaryString
onesToSignMag [] = error "Can't convert an empty binary string"
onesToSignMag (Zero:ds) = (Zero : ds)
onesToSignMag (One:ds) = (One : (map flipBit ds))


-- Add one to a binary string
addOne :: BinaryString -> BinaryString
addOne [] = []
addOne ds =
    let
        ds' = reverse ds
        
        -- Takes the reversed binary string for easier recursion
        addOne' :: BinaryString -> BinaryString
        addOne' [] = []
        addOne' (Zero:ds) = (One : ds)
        addOne' (One:ds) = (Zero : addOne' ds)
    in
        reverse (addOne' ds')


-- Flip sign for Two's Complement
negateTwos :: BinaryString -> BinaryString
negateTwos bs = addOne (map flipBit bs)


-- Convert Two's Complement to One's Complement without first converting to int
-- Satisfies Worksheet 2 Problem 7
twosToOnes :: BinaryString -> BinaryString
twosToOnes [] = error "Can't convert an empty binary string"
twosToOnes (Zero:ds) = (Zero : ds) -- Postive numbers stay the same
twosToOnes ds = map flipBit (negateTwos ds) -- Negative numbers are converted to positive then bits are flipped

-- Convert One's Complement to Two's Complement without first converting to int
-- Satisfies Worksheet 2 Problem 6
onesToTwos :: BinaryString -> BinaryString
onesToTwos (Zero:ds) = (Zero : ds)
onesToTwos ds = negateTwos $ (map flipBit ds)


-- Checks which standard binary string is larger
isGreaterBin :: BinaryString -> BinaryString -> Bool
isGreaterBin [] [] = False
isGreaterBin [x] [y] = x > y
isGreaterBin a b =
     let
        difference = (length a) - (length b)        
        
        xs = if difference > 0 then padZeros a difference else a
        ys = if difference < 0 then padZeros b (negate difference) else b
        
        -- Forms a map of the places where either input mismatches the other
        discrepant = zipWith (\x y -> x /= y) xs ys
        inputsDiffer = or discrepant
        
        -- Finds index of first 'True' in [Bool]
        firstTrueIndex :: [Bool] -> Int
        firstTrueIndex d = length (takeWhile (== False) d)
        
     in
        if inputsDiffer
            -- If the first input has the One at the first discrepancy then it must be greater.
            then (xs !! (firstTrueIndex discrepant)) == One
                else False
        

-- LT comparison for binary strings
isLesserBin :: BinaryString -> BinaryString -> Bool
isLesserBin as bs = (not (isGreaterBin as bs)) && (as /= bs)

-- Greater Than Or Equal comparison
isGTEBin :: BinaryString -> BinaryString -> Bool
isGTEBin as bs = isGreaterBin as bs || as == bs

-- Sort standard binary lowest to highest without converting to int
-- Uses a quicksort like the one defined here https://wiki.haskell.org/Introduction#Quicksort_in_Haskell
sortBin :: [BinaryString] -> [BinaryString]
sortBin [] = []
sortBin (p:xs) = (sortBin lesser) ++ [p] ++ (sortBin greater)
    where
        lesser  = filter (isLesserBin p) xs
        greater = filter (isGTEBin p) xs

-- Sorts signed magnitude binary without converting to int
-- Satisifes Worksheet 1 Problem 11
sortSignMag :: [BinaryString] -> [BinaryString]
sortSignMag [] = error "Can't sort an empty list of binary strings"
sortSignMag [b] = [b]
sortSignMag bs =
    let
        isNegative :: BinaryString -> Bool
        isNegative (x:xs) = x == One
        
        isPositive :: BinaryString -> Bool
        isPositive xs = not (isNegative xs)
        
        pos = filter isPositive bs
        neg = filter isNegative bs
    
    in
    -- Negatives and positives are sorted by magnitude, then concatenated
        sortBin neg ++ sortBin pos


-- Sorts ones complement binary without converting to int
-- Satisfies Worksheet 1 Problem 12
sortOnes :: [BinaryString] -> [BinaryString]
sortOnes [] = error "Can't sort an empty list of binary strings"
sortOnes [b] = [b]
sortOnes bs =
    let
        isNegative :: BinaryString -> Bool
        isNegative (x:xs) = x == One
        
        isPositive :: BinaryString -> Bool
        isPositive xs = not (isNegative xs)

        pos = filter isPositive bs
        neg = filter isNegative bs
    
    in
        reverse (sortBin neg) ++ sortBin pos


slice :: [a] -> Int -> Int -> [a]
slice xs j k
    | j >= (length xs) || k >= (length xs) = error "Index out of bounds"
    | j < 0 || k < 0 = error "Inputs must be positive"
    | otherwise = fst (splitAt (k - (j - 1)) (snd (splitAt j xs)))
        

-- IEEE 754 single precision conversion
-- Does not handle special cases, i.e. infinity
singlePrecisionFloatToFloat :: BinaryString -> Float
singlePrecisionFloatToFloat ds
    | length ds /= 32 = error "Length of binary string representing a single precision float must be 32"
    | otherwise =
        sign * ((2 ^ exponent) * significand)
        where
            bias = 127
            
            sign = if head ds == One then -1 else 1
            
            exponent = excessToInt (slice ds 1 8) bias
            significand = binToFloat (One : (slice ds 9 31)) 0


-- Adaptation of https://stackoverflow.com/a/40746823
binDigitsToStore :: Num -> Num
binDigitsToStore x =
    let
        approx = log x / log 2
        rounded = ceiling approx
    in
        -- Force round up if approximate has no decimal places
        -- Corrects behaviour for exact powers of 2, i.e. corrects x=8 from 3.0 to 4.0
        if approx == rounded then rounded + 1 else rounded
        

-- Requires ability to convert from float to bin generally first
-- floatToSinglePrecisionFloat :: Float -> BinaryString
-- floatToSinglePrecisionFloat n =
    -- let
        -- bias = 127
        -- sign = if x < 0 then 1 else 0
        -- exponent = ((binDigitsToStore n) - 1) + bias
        -- significand = 
            
