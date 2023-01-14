module Exercise09 where


-- Exercise 9.1
-----------------------------------------------------------------------------------------------------------
{- 
Create a function that takes a list of one or more non-negative integers, 
and arranges them such that they form the largest possible number.

Examples:

largestArrangement([4, 50, 8, 145]) returns 8504145 (8-50-4-145)

largestArrangement([4, 40, 7]) returns 7440 (7-4-40)

largestArrangement([4, 46, 7]) returns 7464 (7-46-4)

largestArrangement([5, 60, 299, 56]) returns 60565299 (60-56-5-299)

largestArrangement([5, 2, 1, 9, 50, 56]) returns 95655021 (9-56-5-50-21)
-}
-----------------------------------------------------------------------------------------------------------


import Data.List (sortBy, (\\))

arrange :: [Integer] -> Integer
arrange = read . concat . sortBy comp . map show
                 
comp :: String -> String -> Ordering
comp x y
  | (and $ map (\(v1, v2) -> v1==v2) $ zip x y)
    && length x /= 0 && length y /= 0 = if length x < length y
                                        then comp x (y \\ x)
                                        else comp (x \\ y) y
  | otherwise = compare y x


-- Exercise 9.2
-----------------------------------------------------------------------------------------------------------
{- 
In another dimension, there exist two immortal brothers: Solomon and Goromon. 
As sworn loyal subjects to the time elemental, Chronixus, 
both Solomon and Goromon were granted the power to create temporal folds. 
By sliding through these temporal folds, one can gain entry to parallel dimensions 
where time moves relatively faster or slower.

Goromon grew dissatisfied and one day betrayed Chronixus by stealing the Temporal Crystal, 
an artifact used to maintain the time continuum. 
Chronixus summoned Solomon and gave him the task of tracking down Goromon 
and retrieving the Temporal Crystal.

Using a map given to Solomon by Chronixus, you must find Goromon's precise location.

Mission Details
The map is represented as a 2D array. See the example below:

mapExample = [(1,3,5),(2,0,10),(-3,1,4),(4,2,4),(1,1,5),(-3,0,12),(2,1,12),(-2,2,6)]
Here are what the values of each subarray represent:

Time Dilation: With each additional layer of time dilation entered, time slows by a factor of 2. 
At layer 0, time passes normally. At layer 1, time passes at half the rate of layer 0. 
At layer 2, time passes at half the rate of layer 1, and therefore one quarter the rate of layer 0.
Directions are as follow: 0 = North, 1 = East, 2 = South, 3 = West
Distance Traveled: This represents the distance traveled relative to the current time dilation layer. 
See below:
The following are equivalent distances (all equal a Standard Distance of 100):
Layer: 0, Distance: 100
Layer: 1, Distance: 50
Layer: 2, Distance: 25
For the mapExample above:

mapExample[0] -> [1,3,5]
1 represents the level shift of time dilation
3 represents the direction
5 represents the distance traveled relative to the current time dilation layer

Solomon's new location becomes [-10,0]

mapExample[1] -> [2,0,10]
At this point, Solomon has shifted 2 layers deeper.
He is now at time dilation layer 3.
Solomon moves North a Standard Distance of 80.
Solomon's new location becomes [-10,80]

mapExample[2] -> [-3,1,4]
At this point, Solomon has shifted out 3 layers.
He is now at time dilation layer 0.
Solomon moves East a Standard Distance of 4.
Solomon's new location becomes [-6,80]
Your function should return Goromon's [x,y] coordinates.

For mapExample, the correct output is [346,40].

Additional Technical Details
Inputs are always valid.
Solomon begins his quest at time dilation level 0, at [x,y] coordinates [0,0].
Time dilation level at any point will always be 0 or greater.
Standard Distance is the distance at time dilation level 0.
For given distance d for each value in the array: d >= 0.
Do not mutate the input
-}
-----------------------------------------------------------------------------------------------------------


solomonsQuest :: [(Int,Int,Int)] -> (Int,Int)
solomonsQuest steps = fst $ quest ((0,0),0) steps
  where
    quest (pos,layer) [] = (pos,layer)
    quest (pos,layer) ((dil,dir,dis):ss) = let
        lv = layer + dil
        x = fst pos + (if dir == 1 then dis else if dir == 3 then -dis else 0) * (2^lv)
        y = snd pos + (if dir == 0 then dis else if dir == 2 then -dis else 0) * (2^lv)
      in quest ((x,y),lv) ss


-- Exercise 9.3
-----------------------------------------------------------------------------------------------------------
{-
You get a polygon as a list of 2D coordinates defining its vertices. 
The edges of the polygon are assumed to be drawn between adjacent elements of the list, 
with the polygon closing by an edge between the last element and the first.

Your task is to determine whether this polygon is convex. Convexity can be defined in several different ways:

All internal angles of the polygon are 180 degrees or smaller.
For each edge, all the vertices of the polygon are on the same side of the line defined by that edge.
If we walk along the edges of the polygon, going through all the vertices, we always turn in the same direction.
Any line drawn through the polygon crosses at most two edges.
For example:

convex [(-1,-1), (-1,1), (1,1), (1,-1)] == True

convex [(0,0), (-1,-1), (-1,1), (1,1), (1,-1)] == False
By convention, we consider a polygon with fewer than 4 vertices to be convex, including the empty polygon.
-}
-----------------------------------------------------------------------------------------------------------


convex :: (Num a, Ord a) => [(a,a)] -> Bool
convex [] = True
convex [_] = True
convex (x:y:xs) =
  and (zipWith3 isLeft (x:y:xs) (y:xs++[x]) (xs++[x,y])) ||
  and (zipWith3 isRight (x:y:xs) (y:xs++[x]) (xs++[x,y]))

isLeft :: (Num a, Ord a) => (a,a) -> (a,a) -> (a,a) -> Bool
isLeft (x0,y0) (x1,y1) (x2,y2) = (x1-x0)*(y2-y1) - (x2-x1)*(y1-y0) >= 0

isRight :: (Num a, Ord a) => (a,a) -> (a,a) -> (a,a) -> Bool
isRight a b c = isLeft c b a


-- Exercise 9.4
-----------------------------------------------------------------------------------------------------------
{- 
If we alternate the vowels and consonants in the string "have", 
we get the following list, arranged alphabetically: 
['ahev', 'aveh', 'ehav', 'evah', 'have', 'heva', 'vahe', 'veha']. 
These are the only possibilities in which vowels and consonants are alternated. 
The first element, ahev, is alphabetically lowest.

Given a string:

alternate the vowels and consonants and return the lexicographically lowest element in the list
If any two or more vowels or consonants must follow each other, return "failed"
if the number of vowels and consonants are equal, the first letter of the result must be a vowel.
Examples:

solve("codewars") = "failed". However you alternate vowels and consonants, two consonants must follow each other
solve("oruder") = "edorur"
solve("orudere") = "ederoru". This is the only option that allows you to alternate vowels & consonants.
Vowels will be any of "aeiou". Input will be a lowercase string, no spaces. See test cases for more examples.
-}
-----------------------------------------------------------------------------------------------------------


import Data.List(sort,(\\))

solve :: [Char] -> [Char]
solve xs 
  | abs (countV - countC) > 1 = "failed"
  | countV == countC = combine vowels constants
  | countC > countV = combine constants vowels 
  | otherwise = combine vowels constants where 
  countV = length vowels
  vowels = sort $ filter (`elem` "aeiou") xs
  countC = length constants 
  constants = sort $ xs \\ vowels
  combine [] ys = ys
  combine xs [] = xs
  combine (x:xs) (y:ys) =  x:y:combine xs ys


-- Exercise 9.5
-----------------------------------------------------------------------------------------------------------
{- 
You are a lonely frog.

You live on a coordinate axis.

The meaning of your life is to jump and jump..

Two actions are allowed:

forward: Move forward 1 unit.

double: If you at x point, then you can move to x*2 point.

Now, here comes your new task. Your starting point is x, the target point is y.

You need to jump to the target point with minimal steps. Please tell me, what's the minimal steps?

1 <= x <= 10, x < y <= 100000

Example
For x = 1, y = 8, the output should be 3.

 step  from   to      action
  1:     1 --> 2     forward(or double)
  2:     2 --> 4       double
  3:     4 --> 8       double
For x = 1, y = 17, the output should be 5.

 step  from    to      action
  1:     1  --> 2     forward(or double)
  2:     2  --> 4       double
  3:     4  --> 8       double
  4:     8  --> 16      double
  5:     16 --> 17     forward
For x = 1, y = 15, the output should be 6.

 step  from    to      action
  1:     1  --> 2      forward(or double)
  2:     2  --> 3      forward
  3:     3  --> 6      double
  5:     6  --> 7      forward
  6:     7  --> 14     double
  7:     14 --> 15     forward
For x = 3, y = 12, the output should be 2.

 step  from    to       action
  1:     3  --> 6       double
  2:     6  --> 12      double
For x = 3, y = 16, the output should be 3.

 step  from    to       action
  1:     3  --> 4      forward
  2:     4  --> 8       double
  3:     8  --> 16      double
-}
-----------------------------------------------------------------------------------------------------------


import Data.Array

tabulate :: (Ix i) => (i->e) -> (i,i) -> Array i e
tabulate f bounds = listArray bounds (map f (range bounds))

jumpTo :: Int -> Int -> Int
jumpTo x y = tbs ! y
  where tbs = tabulate f (x,y)
        f i = if i==x then 0 else
              if even i && i>=2*x then 1+min (tbs!(i-1)) (tbs!(div i 2)) else
              1+ tbs!(i-1)


-- Exercise 9.6
-----------------------------------------------------------------------------------------------------------
{- 
you will create a function, circle, that produces a string of some radius, 
according to certain rules that will be explained shortly. 
Here is the output of circle when passed the integer 10:

     █████████     
    ███████████    
  ███████████████  
  ███████████████  
 █████████████████ 
███████████████████
███████████████████
███████████████████
███████████████████
███████████████████
███████████████████
███████████████████
███████████████████
███████████████████
 █████████████████ 
  ███████████████  
  ███████████████  
    ███████████    
     █████████     
(Note: For Python and Ruby users, this will use '#', rather than '█')

The circle consists of spaces, and the character \u2588. 
Note that this is a complete square of characters, so the 'gaps' are filled with spaces. 
For instance, the last line of the above ends with the five characters "\u2588     "; 
there are five spaces at the end.

All characters whose distance from the center is less than the given radius is defined as 'in the circle', 
hence the character at that position should be filled with \u2588 rather than spaces. 
So, for instance, this is a circle of radius 2:

███
███
███
Whilst this isn't very circle-y, this is what the rules expect.

Here are the edge-case rules; there are examples in the example test cases:

A negative radius should result in an empty string.
A radius of 0 should produce the string "\n:.
Any other result should end with \n.
Please note that the distance metric is Euclidean distance (the most common notion of distance) 
centered around the middle of a character, where each character is of width and height exactly one.
-}
-----------------------------------------------------------------------------------------------------------


import Data.List.Split (chunksOf)

circle :: Int -> String
circle 0 = "\n"
circle r = unlines $ chunksOf n $ map (mapf m r) [(a,b) | a <- [0..(n-1)] , b <- [0..(n-1)]]
  where n = 2*r - 1
        m = r-1
        
mapf :: Int -> Int -> (Int,Int) -> Char
mapf m r (x,y)
  | dx*dx + dy*dy < r*r = '█'
  | otherwise = ' '
  where dx = x - m
        dy = y - m


-- Exercise 9.7
-----------------------------------------------------------------------------------------------------------
{-
An array is defined to be odd-heavy if it contains at least one odd element 
and every element whose value is odd is greater than every even-valued element.

eg.

Array [11,4,9,2,8] is odd-heavy 
because:- its odd elements [11,9] are greater than all the even elements [4,2,8]

Array [11,4,9,2,3,10] is not odd-heavy
because:- one of it's even element 10 from [4,2,10] is greater than two of its odd elements [9,3] from [ 11,9,3]
write a function called isOddHeavy or is_odd_heavy that accepts an integer array 
and returns true if the array is odd-heavy else return false.
-}
-----------------------------------------------------------------------------------------------------------


splitBy :: (a -> Bool) -> [a] -> ([a], [a])
splitBy _ [] = ([], [])
splitBy f (x : xs) 
    | f x = (x : a, b)
    | otherwise  = (a, x : b)
    where (a, b) = splitBy f xs

isOddHeavy :: [Int] -> Bool 
isOddHeavy xs = 
    let (o, e) = splitBy odd xs 
    in not (null o) && (
        null e ||
        (minimum o) > (maximum e)
    )


-- Exercise 9.8
-----------------------------------------------------------------------------------------------------------
{- 
Given a lowercase string that has alphabetic characters only and no spaces, 
return the highest value of consonant substrings. 
Consonants are any letters of the alphabet except "aeiou".

We shall assign the following values: a = 1, b = 2, c = 3, .... z = 26.

For example, for the word "zodiacs", let's cross out the vowels. We get: "z o d ia cs"

-- The consonant substrings are: "z", "d" and "cs" and the values are z = 26, d = 4 
and cs = 3 + 19 = 22. The highest is 26.
solve("zodiacs") = 26

For the word "strength", solve("strength") = 57
-- The consonant substrings are: "str" and "ngth" with values "str" = 19 + 20 + 18 = 57 
and "ngth" = 14 + 7 + 20 + 8 = 49. The highest is 57.
For C: do not mutate input.
-}
-----------------------------------------------------------------------------------------------------------



import Data.List.Split 
import Data.Char

consons :: String -> [String]
consons = wordsBy $ flip elem "aeiou"

consonValue :: String -> Int
consonValue = sum . map (subtract 96 . ord)

solve :: String -> Int
solve = maximum . map consonValue . consons


-- Exercise 9.9
-----------------------------------------------------------------------------------------------------------
{- 
A Pythagorean triplet is a set of three numbers a, b, and c where a^2 + b^2 = c^2. 
you will be tasked with finding the Pythagorean triplets whose product is equal to n, 
the given argument to the function pythagorean_triplet.

Your task
you will be tasked with finding the Pythagorean triplets whose product is equal to n, 
the given argument to the function, where 0 < n < 10000000

Examples
One such triple is 3, 4, 5. For this challenge, 
you would be given the value 60 as the argument to your function, 
and then it would return the Pythagorean triplet in an array [3, 4, 5] 
which is returned in increasing order. 3^2 + 4^2 = 5^2 since 9 + 16 = 25 
and then their product (3 * 4 * 5) is 60.

More examples:

argument      returns
60          [3, 4, 5]
780         [5, 12, 13]
2040        [8, 15, 17]
-}
-----------------------------------------------------------------------------------------------------------


import Data.Maybe

pythagoreanTriplet :: Int -> [Int]
pythagoreanTriplet n = f n 4

f :: Int -> Int  -> [Int]
f n b | x == Nothing = f n (b+1)
      | otherwise = fromJust x
        where x = g n b 3

g :: Int -> Int -> Int -> Maybe [Int]
g n b a | a == b = Nothing
        | n^2 == (a^2 + b^2) * (a*b)^2 = Just [a,b,div n (a*b)]
        | otherwise = g n b (a+1)


-- Exercise 9.10
-----------------------------------------------------------------------------------------------------------
{- 
Given 3 points a, b, c

            c
            
                     b         
  a

Find the shortest distance from point c to a straight line that passes through points a and b

Notes

all points are of the form [x,y] where x >= 0 and y >= 0
if a and b are the same then just return distance between a and c
use Euclidean distance
-}
-----------------------------------------------------------------------------------------------------------



type Point = (Double, Double)

distanceFromLine :: Point -> Point -> Point -> Double
distanceFromLine a@(aₓ, aᵧ) b@(bₓ, bᵧ) c@(cₓ, cᵧ)
  | a == b = dist c a
  | aₓ == bₓ = abs (cₓ - aₓ)
  | aᵧ == bᵧ = abs (cᵧ - aᵧ)
  | otherwise =
    let m₁ = (bᵧ - aᵧ) / (bₓ - aₓ)
        b₁ = aᵧ - m₁ * aₓ
        m₂ = - recip m₁
        b₂ = cᵧ - m₂ * cₓ
        dₓ = (b₂ - b₁) / (m₁ - m₂)
        dᵧ = m₂ * dₓ + b₂
     in dist c (dₓ, dᵧ)
  where
    dist (aₓ, aᵧ) (bₓ, bᵧ) = sqrt ((aₓ - bₓ) ** 2 + (aᵧ - bᵧ) ** 2)


-- Exercise 9.11
-----------------------------------------------------------------------------------------------------------
{- 
Write a function that returns the smallest distance between two factors of a number. 
The input will always be a number greater than one.

Example:

13013 has factors: [ 1, 7, 11, 13, 77, 91, 143, 169, 1001, 1183, 1859, 13013]

Hence the asnwer will be 2 (=13-11)
-}
-----------------------------------------------------------------------------------------------------------


factors :: Integral a => a -> [a]
factors n = [x | x <- [1 .. n], n `mod` x == 0]

pairwise :: [a] -> [(a, a)]
pairwise xs = zip xs $ tail xs

minDistance :: Integer -> Integer
minDistance = minimum . map (\(x, y) -> y - x) . pairwise . factors


-- Exercise 9.12
-----------------------------------------------------------------------------------------------------------
{- 
A twin prime is a prime number that differs from another prime number by 2. 
Write a function named is_twin_prime which takes an int parameter 
and returns true if it is a twin prime, else false.

Examples

given 5, which is prime
5 + 2 = 7, which is prime 
5 - 2 = 3, which is prime
hence, 5 has two prime twins and it is a Twin Prime.

given 7, which is prime
7 - 2 = 5, which is prime
7 + 2 = 9. which is not prime
hence, 7 has one prime twin, and it is a Twin Prime.

given 9, which is not prime 
hence, 9 is not a Twin Prime

given 953, which is prime
953 - 2 = 951, which is not prime
953 + 2 = 955, which is not prime 
hence, 953 is not a Twin Prime
-}
-----------------------------------------------------------------------------------------------------------


isTwinPrime :: Integer -> Bool
isTwinPrime n
  | n < 3 = False
  | not . isPrime $ n = False
  | isPrime (n - 2) = True
  | otherwise = isPrime (n + 2)

primes :: [Integer]
primes = (initialPrimes ++) . filter isPrime $ [11, 13 ..]
  where
    initialPrimes = [1, 2, 3, 5, 7]

isPrime :: Integer -> Bool
isPrime n = not . any ((== 0) . (n `mod`)) . takeWhile (<= root) . tail $ primes
  where
    root = integerRoot 2 n :: Integer

integerRoot :: Integer -> Integer -> Integer
integerRoot n base
  | d < e = d
  | otherwise = e
  where
    n1 = n - 1
    (d, e) = search 1 ((n1 + base) `div` n)

    search :: Integer -> Integer -> (Integer, Integer)
    search c d'
      | c /= d' && c /= e' = search d' e'
      | otherwise = (d', e')
      where
        e' = (n1 * d' + (base `div` d' ^ n1)) `div` n


-- Exercise 9.13
-----------------------------------------------------------------------------------------------------------
{- 
Consider the range 0 to 10. The primes in this range are: 2, 3, 5, 7, 
and thus the prime pairs are: (2,2), (2,3), (2,5), (2,7), (3,3), (3,5), (3,7),(5,5), (5,7), (7,7).

Let's take one pair (2,7) as an example and get the product, 
then sum the digits of the result as follows: 2 * 7 = 14, and 1 + 4 = 5. 
We see that 5 is a prime number. Similarly, for the pair (7,7), we get: 7 * 7 = 49, and 4 + 9 = 13, 
which is a prime number.

You will be given a range and your task is to return the number of pairs that revert to prime as shown above. 
In the range (0,10), there are only 4 prime pairs that end up being primes in a similar way: 
(2,7), (3,7), (5,5), (7,7). Therefore, solve(0,10) = 4)

Note that the upperbound of the range will not exceed 10000. A range of (0,10) means that: 0 <= n < 10.

Good luck!
-}
-----------------------------------------------------------------------------------------------------------


import Data.List

primes = sieve [2..]
sieve (p:ps) = p : sieve [x | x <- ps, mod x p /= 0]

primeRange :: Int -> Int -> [Int]
primeRange a b = [x | x <- takeWhile (< b) primes, x >= a]

primePairs :: Int -> Int -> [(Int,Int)]
primePairs a b = nub [(x, y) | x <- primeRange a b, y <- primeRange a b, x <= y]

sumDigits 0 = 0
sumDigits x = (x `mod` 10) + sumDigits (x `div` 10)

revertPair :: (Int, Int) -> Int
revertPair x = sumDigits (fst x * snd x)

checkValid :: (Int, Int) -> Bool
checkValid x = revertPair x `elem` take (revertPair x) primes

solve :: Int -> Int -> Int
solve a b =  length [x | x <- primePairs a b, checkValid x]


-- Exercise 9.14
-----------------------------------------------------------------------------------------------------------
{- 
The depth of an integer n is defined to be how many multiples of n 
it is necessary to compute before all 10 digits have appeared at least once in some multiple.

example:

let see n=42

Multiple         value         digits     comment
42*1              42            2,4 
42*2              84             8         4 existed
42*3              126           1,6        2 existed
42*4              168            -         all existed
42*5              210            0         2,1 existed
42*6              252            5         2 existed
42*7              294            9         2,4 existed
42*8              336            3         6 existed 
42*9              378            7         3,8 existed

Looking at the above table under digits column you can find all the digits from 0 to 9, 
Hence it required 9 multiples of 42 to get all the digits. 
So the depth of 42 is 9. Write a function named computeDepth which computes the depth of its integer argument.
Only positive numbers greater than zero will be passed as an input.
-}
-----------------------------------------------------------------------------------------------------------


import Data.List

computeDepth :: Int -> Int 
computeDepth d = calcDepth d 1 []

calcDepth :: Int -> Int -> [Int] -> Int
calcDepth d de ds
  | length (nub ds) == 10 = de - 1
  | otherwise = calcDepth d (de + 1) (ds ++ splitNumberToList (d * de))

splitNumberToList :: Int -> [Int]
splitNumberToList d | d > 0     = splitNumberToList (d `div` 10) ++ [d `mod` 10]
                    | otherwise = []


-- Exercise 9.15
-----------------------------------------------------------------------------------------------------------
{- 
A number n is called prime happy if there is at least one prime less than n 
and the sum of all primes less than n is evenly divisible by n. 
Write isPrimeHappy(n) which returns true if n is prime happy else false.
-}
-----------------------------------------------------------------------------------------------------------


isPrime :: Int -> Bool
isPrime n = n > 1 && go n 2
    where go n k
              | k * k > n      = True
              | n `mod` k == 0 = False
              | otherwise      = go n (k + 1)

isPrimeHappy :: Int -> Bool
isPrimeHappy n = n >= 5 && ((== 0) . flip mod n . sum . filter isPrime $ [2 .. n - 1])


-- Exercise 9.16
-----------------------------------------------------------------------------------------------------------
{- 
Write a method named getExponent(n,p) that returns the largest integer exponent x such that px evenly divides n. 
if p<=1 the method should return null/None (throw an ArgumentOutOfRange exception in C#).
-}
-----------------------------------------------------------------------------------------------------------



import Control.Arrow ((***))

getExponent :: Integer -> Integer -> Maybe Integer
getExponent 0 p = Nothing
getExponent n p
  | p <= 1 = Nothing
  | otherwise =
      Just $ snd $
      until
        ( (/= 0) . (`mod` p) . fst)
        ( (`div` p) *** (+1) )
        (n,0)


-- Exercise 9.17
-----------------------------------------------------------------------------------------------------------
{- 
An array is called centered-N if some consecutive sequence of elements of the array sum to N 
and this sequence is preceded and followed by the same number of elements.

Example:

[3,2,10,4,1,6,9] is centered-15
because the sequence 10,4,1 sums to 15 and the sequence 
is preceded by two elements [3,2] and followed by two elements [6,9]
Write a method called isCenteredN that returns :

true if its array argument is not empty and centered-N or empty and centered-0
otherwise returns false.
-}
-----------------------------------------------------------------------------------------------------------


isCentered :: [Int] -> Int -> Bool
isCentered xs n | l == 0 = False
                | (n == 0 && r*(xs !! q) == 0) || s == n = True
                | otherwise = f xs s n
                  where
                  l = length xs
                  (q,r) = divMod l 2
                  s = sum xs

f :: [Int] -> Int -> Int -> Bool
f [] _ _  = False
f a s n | x == n = True
        | length a == 1 = x == n
        | otherwise = f (tail $ init a) x n
          where x = s - (head a) - (last a)


-- Exercise 9.18
-----------------------------------------------------------------------------------------------------------
{- 
A Madhav array has the following property:

a[0] = a[1] + a[2] = a[3] + a[4] + a[5] = a[6] + a[7] + a[8] + a[9] = ...

Complete the function/method that returns true if the given array is a Madhav array, otherwise it returns false.

Edge cases: An array of length 0 or 1 should not be considered a Madhav array as there is nothing to compare.
-}
-----------------------------------------------------------------------------------------------------------


isSquare :: Integral n => n -> Bool
isSquare = f . sqrt . fromIntegral
  where
    f :: Double -> Bool
    f n = n == fromInteger (round n)

isTriangular :: Integral n => n -> Bool
isTriangular = isSquare . (+1) . (*8)

isMadhavArray :: [Int] -> Bool
isMadhavArray []  = False
isMadhavArray [_] = False
isMadhavArray xs  = isTriangular (length xs) && let ys = f 1 xs in all (== head ys) ys
  where
    f :: Int -> [Int] -> [Int]
    f n [] = []
    f n xs = (sum . take n) xs : f (n + 1) (drop n xs)


-- Exercise 9.19
-----------------------------------------------------------------------------------------------------------
{- 
The sum of the primes below or equal to 10 is 2 + 3 + 5 + 7 = 17. 
Find the sum of all the primes below or equal to the number passed in.
-}
-----------------------------------------------------------------------------------------------------------


sumOfPrimes :: Int -> Int
sumOfPrimes n = sum $ primes n

primes :: Int -> [Int]
primes n | n<2  = [] 
         | n<4  = [2..n]
         | True = primes (pred n) ++ [n | isPrime n]
         
isPrime n = n > 1 && (n < 4 || and [rem n p /= 0 | p <- 2 : [3,5..(div n 2) + 2]])


-- Exercise 9.20
-----------------------------------------------------------------------------------------------------------
{- 
Consider the prime number 23. If we sum the square of its digits 
we get: 2^2 + 3^2 = 13, then for 13: 1^2 + 3^2 = 10, and finally for 10: 1^2 + 0^2 = 1.

Similarly, if we start with prime number 7, the sequence is: 7->49->97->130->10->1.

Given a range, how many primes within that range will eventually end up being 1?

The upperbound for the range is 50,000. A range of (2,25) means that: 2 <= n < 25.

Good luck!
-}
-----------------------------------------------------------------------------------------------------------


import Data.Char

solve :: Int -> Int -> Int
solve a b = sum $ map(\m -> f m m []) $ filter isPrime [a..b-1]

f :: Int -> Int -> [Int] -> Int
f m x a | t == 1 = 1
        | elem t a = 0
        | otherwise = f m t (a++[t])
          where t = sum $ map(\d -> (digitToInt d)^2) $ show x

factors :: Int -> [Int]
factors x = [n | n <- [3, 5 ..  s ], mod x n == 0]
            where s = ceiling (sqrt (fromIntegral x) ) 

isPrime :: Int -> Bool
isPrime x
  | x <2 = False
  | x==2 = True
  | mod x 2 == 0 = False
  | x>=2 = factors x == [] 
