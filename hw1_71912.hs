-- Task 1
solveQuadratic :: Double -> Double -> Double -> (Double, Double)
solveQuadratic a b c
  | d < 0  = error "0" 
  | otherwise = (x1, x2)
  where
   x1 = (smallNum + sqrt d)/2 * a
   x2 = (smallNum - sqrt d)/2 * a 
   smallNum = - (b / (2 * a))
   d = b * b - 4 * a * c
  

-- Task 2
sumPrimes :: Integer -> Integer -> Integer
sumPrimes n k
 | k == 0 = 0
 | isPrime n = n + sumPrimes (n + 1) (k - 1) 
 | otherwise = sumPrimes (n + 1) k
 where
  isPrime :: Integer -> Bool
  isPrime num = helpPrime num 2
  where
   helpPrime :: Integer -> Integer -> Bool
   helpPrime num i 
    | i == num = True
    | mod num i == 0 = False
    | otherwise = helpPrime num (i + 1)


-- Task 3
countPalindromes :: Integer -> Integer -> Integer
countPalindromes a b = palindrome a b 0
where
 palindrome a b i 
 | a == b + 1 = i 
  | isPldrm a == True = palindrome (a + 1) b (i + 1)
  | otherwise = palindrome (a + 1) b i
  where
   isPldrm :: Integer -> Bool
   isPldrm a 
    | reverse a 0 == a = True
    | otherwise = False
    where
     reverse :: Integer -> Integer
      | reverse a reversed
      | a == 0 = reversed 
      | otherwise = reverse (div a 10) (10*reversed + (mod a 10)) 


-- Task 4
truncatablePrime :: Integer -> Bool
truncatablePrime num
 | num == 0 = True
 | isPrime num == False = False
 | otherwise = truncatablePrime (div num 10)
 