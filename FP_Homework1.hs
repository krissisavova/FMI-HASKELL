{-
Задача 1. Нека е дадено квадратно уравнение аx2
+ bx + c = 0 , където a, b и c са реални
числа. Дефинирайте функция solveQuadratic :: Double -> Double -> Double -> (Double,
Double), която получава като аргументи, коефициентите a, b и c и връща двойката
решения на уравнението или индикация за грешка, ако дискриминантата на уравнението
е отрицателна.

Задача 2. Дефинирайте функция sumPrimes :: Integer -> Integer -> Integer , която приема
целите числа n и k и връща сумата на първите k прости числа, по-големи или равни на n.

Задача 3. Ще наричаме едно цяло положително число палиндром, ако то е равно на
числото, записано със същите цифри, но в обратен ред (приемаме, че числата са
дефинирани в десетична бройна система).
Дефинирайте функция countPalindromes :: Integer -> Integer -> Integer, която приема
аргументи a и b и връща броя на палиндромите в целочисления интервал [a, b], a ≤ b.

Задача 4. Дефинирайте предикат truncatablePrime :: Integer -> Bool , който връща
стойност True точно когато аргументът num притежава едновременно следните свойства:
● числото num е просто;
● всички числа, които се получават чрез премахване на цифри в края на num, също
са прости.
Примери:
truncatablePrime 3797 ⇢ True (числата 3797, 379, 37 и 3 са прости)
truncatablePrime 47 ⇢ False
-}

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
 
