module Exercise05 where


-- Exercise 5.1
-----------------------------------------------------------------------------------------------------------
{- 
With Cereal crops like wheat or rice, before we can eat the grain kernel, 
we need to remove that inedible hull, or to separate the wheat from the chaff.

Task
Given a sequence of n integers , separate the negative numbers (chaff) from positive ones (wheat).!alt

Notes
Sequence size is at least 3
Return a new sequence, such that negative numbers (chaff) come first, then positive ones (wheat).
In Java , you're not allowed to modify the input Array/list/Vector
Have no fear , it is guaranteed that there will be no zeroes .!alt
Repetition of numbers in the input sequence could occur , so duplications are included when separating.
If a misplaced positive number is found in the front part of the sequence, 
replace it with the last misplaced negative number (the one found near the end of the input). 
The second misplaced positive number should be swapped with the second last misplaced negative number. 
Negative numbers found at the head (begining) of the sequence , should be kept in place .
Input >> Output Examples:
wheatFromChaff ({7, -8, 1 ,-2}) ==> return ({-2, -8, 1, 7}) 
Explanation:
Since 7  is a positive number , it should not be located at the beginnig 
so it needs to be swapped with the last negative number -2.
wheatFromChaff ({ -31, -5, 11 , -42, -22, -46, -4, -28 }) ==> return ({-31, -5,- 28, -42, -22, -46 , -4, 11})
Explanation: 
Since, {-31, -5}  are negative numbers found at the head (begining) of the sequence , so we keep them in place .
Since 11 is a positive number, it's replaced by the last negative which is -28 , and so on till sepration is complete.
wheatFromChaff ({ -25, -48, -29, -25, 1, 49, -32, -19, -46, 1}) ==> return ({-25, -48, -29, -25, -46, -19, -32, 49, 1, 1})
Explanation:
Since { -25, -48, -29, -25}  are negative numbers found at the head (begining) of the input , so we keep them in place .

Since 1 is a positive number, it's replaced by the last negative which is -46 , and so on till sepration is complete.

Remeber, duplications are included when separating , that's why the number 1 appeared twice at the end of the output.
-}-}-}-}-}
-----------------------------------------------------------------------------------------------------------


import Data.Sequence (Seq(..), singleton, fromList)
import Data.Foldable

wheatFromChaff :: [Int] -> [Int]
wheatFromChaff = toList . treat . fromList

treat Empty = Empty
treat (x :<| Empty) = singleton x
treat ((x :<| xs) :|> y)
  | x < 0 = x :<| treat (xs :|> y)
  | y > 0 = treat (x :<| xs) :|> y
  | otherwise = (y :<| treat xs) :|> x


-- Exercise 5.2
-----------------------------------------------------------------------------------------------------------
{- 
You are given three non negative integers a, b and n, 
and making an infinite sequence just like fibonacci sequence, use the following rules:

step 1: use ab as the initial sequence.
step 2: calculate the sum of the last two digits of the sequence, and append it to the end of sequence.
repeat step 2 until you have enough digits
Your task is to complete the function which returns the nth digit (0-based) of the sequence.

Notes:
0 <= a, b <= 9, 0 <= n <= 10^10
16 fixed testcases
100 random testcases, testing for correctness of solution
100 random testcases, testing for performance of code
All inputs are valid.
Pay attention to code performance.
Examples
For a = 7, b = 8 and n = 9 the output should be 5, because the sequence is:

78 -> 7815 -> 78156 -> 7815611 -> 78156112 -> 781561123 -> 7815611235 -> ...
and the 9th digit of the sequence is 5.

For a = 0, b = 0 and n = 100000000 the output should be 0, because all the digits in this sequence are 0.
-}
-----------------------------------------------------------------------------------------------------------


seq1 = [1,4,5,9]
seq2 = [1,1,2,3,5,8,1,3,4,7]

find :: Int -> Int -> Int -> Int
find 0 0 _ = 0
find a b 0 = a
find a b 1 = b

find 0 7 n = find'' 0 7 n 3 seq1
find 1 4 n = seq1 !! (n `mod` 4)
find 2 6 n = find'' 2 6 n 3 seq1
find 3 1 n = find'' 3 1 n 1 seq1
find 4 2 n = find'' 4 2 n 4 seq1
find 4 5 n = find'' 4 5 n 3 seq1
find 5 9 n = find'' 5 9 n 2 seq1
find 6 8 n = find'' 6 8 n 2 seq1
find 7 0 n = find'' 7 0 n 4 seq1
find 7 7 n = find'' 7 7 n 2 seq1
find 8 6 n = find'' 8 6 n 2 seq1
find 9 5 n = find'' 9 5 n 2 seq1
find 9 1 n = find'' 9 1 n 4 seq2

find a b n = find'' a b n i seq2
             where i = find11 xs
                   xs = head $ dropWhile (\f -> (length f) <= 30) (fibs [a,b])
                   fibs = iterate f
find11 (1:1:xs) = 0
find11 (a:b:xs) = 1+find11(b:xs)

find'' a b n i seq = if n < i then find' a b n else seq !! ((n-i) `mod` (length seq))

find' a b n = xs !! n
              where xs = head $ dropWhile (\f -> (length f) <= n) (fibs [a,b])
                    fibs = iterate f 

f xs = if c <= 9
       then xs ++ [c]
       else xs ++ [1,c-10] 
       where a = (last.init) xs
             b = last xs
             c = a+b


-- Exercise 5.3
-----------------------------------------------------------------------------------------------------------
{-
Given an array [x1, x2, ..., xn] determine whether it is possible to put + or - 
between the elements and get an expression equal to sum. Result is boolean

2 <= n <= 22
0 <= xi <= 20
-10 <= sum <= 10
Example
arr = [1, 3, 4, 6, 8]

sum = -2

1 + 3 - 4 + 6 - 8 = -2

Result is: true

Notes
If it is impossible to find a solution then you should return false.
-}
-----------------------------------------------------------------------------------------------------------


import Prelude hiding (length)
import Data.Vector.Unboxed (fromList,(!),length)

solve :: [Int] -> Int -> Bool
solve (x:xs) sum = go 0 (sum-x) where
  vector = fromList xs
  go = memo $ \ i -> memo $ \ n ->
    if i == length vector then
      n == 0
    else
      go (i+1) (n - vector!i) || go (i+1) (n + vector!i)

-- credit: dramforever

data Tree a = Tree a (Tree a) (Tree a) deriving (Functor)

memo :: (Enum a) => (a -> b) -> (a -> b)
memo fn = find (fn . toEnum <$> cache) . fromEnum where
  cache :: Tree Int
  cache = Tree 0 (negate <$> tr) tr where tr = Tree 1 ((*2) <$> tr) ((+1) . (*2) <$> tr)
  find :: Tree a -> Int -> a
  find (Tree zero neg pos) n
    | n == 0 = zero
    | n < 0 = go neg (negate n)
    | otherwise = go pos n
    where go (Tree one _ _) 1 = one
          go (Tree _ teven todd) k = go next (k `div` 2) where next = if even k then teven else todd


-- Exercise 5.4
-----------------------------------------------------------------------------------------------------------
{- 
Given an integer n, find two integers a and b such that:

A) a >= 0 and b >= 0
B) a + b = n
C) DigitSum(a) + Digitsum(b) is maximum of all possibilities.  
You will return the digitSum(a) + digitsum(b).

For example:
solve(29) = 11. If we take 15 + 14 = 29 and digitSum = 1 + 5 + 1 + 4 = 11. There is no larger outcome.
n will not exceed 10e10.
-}
-----------------------------------------------------------------------------------------------------------



solve :: Int -> Int 
solve 0 = 0
solve n = a + b + solve c
  where
    a = if n > 9 then 9 else n
    b = (n - a) `mod` 10
    c = (n - a - b) `div` 10


-- Exercise 5.5
-----------------------------------------------------------------------------------------------------------
{- 
Dwight and Mos are back at the beets, but this time trouble is afoot! 
Someone has interrupted the packing and stolen a bundle of beets!

Mos was up all night and figured out which bag was stolen 
by carefully comparing the bags which remain (which are numbered by the amount of beets they contain) 
to his original list.

However before he passed out on fermented beet juice all he wrote down was that 
it was the smallest amount of beets he had not originally packed, which he thought was an amazing coincidence.

Now Dwight has to take Mos's record of beet bags and figure out what is missing. 
If Mos has sorted the list it would be easy, but Mos didn't sort the list.

Is there a decent way for Dwight to find the smallest missing bag number quickly?

there are no duplicates
they are all positive int's, including 0
the random lists are big and there are a lot of them
minfree [0,1,3,4,5] => 2
minfree [6,1,4,5,0] => 2
-}
-----------------------------------------------------------------------------------------------------------


minfree :: [Int] -> Int
minfree x = help x 0 

help :: [Int] -> Int -> Int
help [] n = n
help (x:xs) n = if (pr (x:xs) n) then help xs (n+1) else n
             
              
pr::[Int]->Int->Bool
pr [] n= False
pr (x:xs) n = if (x==n) then True else pr xs n


-- Exercise 5.6
-----------------------------------------------------------------------------------------------------------
{- 
We have a coordinate plane and an array of points (x,y).
You should create a function, which takes the array and returns 
either the X-coordinate of the vertical axis of symmetry, if it exists for these points, or null.

    |
 * *|* * 
*   |   *
  * * *
    |
should return its X

      *
  * *  *                *
*    *
   ***
should return null

notes
Point can be on axis of symmetry
Multiple points can have the same coordinates. For every such point we must have a mirror point, or return null
For empty array return 0 ( because (0,0) is the center of the coordinate plane )
Is it possible to solve it in O(n)?
for Haskell
Return data type should be Maybe Int
Round down to a whole number ( -4.5 becomes -5 )
-}
-----------------------------------------------------------------------------------------------------------


import Data.List

checkVerticalSymmetry :: [(Int,Int)] -> Maybe Int
checkVerticalSymmetry  p | null p = Just 0
                         | otherwise = f a (fromIntegral $ fst $ head a)
                          where
                          a = sortOn fst p
                          
f :: [(Int,Int)] -> Float -> Maybe Int
f a x | lr < ll = Nothing
      | lr /= lr = f a (x+0.5)
      | (sort $ map(\(m,n) -> stringify (x-(fromIntegral m),n)) left) == (sort $ map(\(m,n) -> stringify ((fromIntegral m)-x,n)) right) = Just $ floor x
      | otherwise = f a (x + 0.5)
        where
        left = filter(\(m,n) -> (fromIntegral m) < x) a
        right = filter(\(m,n) -> (fromIntegral m) > x) a
        lr = length right
        ll = length left
        stringify (a,b) = (show a) ++ " " ++ (show b)


-- Exercise 5.7
-----------------------------------------------------------------------------------------------------------
{-
I was doing a coding challenge. It was one of those multi-step challenges. 
I don't know if my approach was good or bad, 
but in one of these steps I was writing a function to convert word to numbers. 
I did it.. eventually, but... I didn't like how it was written. 
So I thought why not create kata and check how other people do it :) So:

Task:
Your task is to write the word to number converter. 
Digits in the number should match letters in the word. 
Plus generated number should be the smallest possible number you can get.

Words will contain of maximum 10 distinct letters, 
but word can be any length, even longer than 10 characters long.
Number can NOT start with 0
Same letters share the same digit regardless of case
For empty string return 0
Examples:
"A" -> 1 - OK

"ABA" -> 353 - WRONG ( number is OK, but it's not the smallest number )

"ABA" -> 333 - WRONG ( different letters map to same digits )

"ABA" -> 357 - WRONG ( same letters map to different digits )
-}
-----------------------------------------------------------------------------------------------------------


import Control.Applicative (liftA2)
import Data.Char (toLower)
import Data.List (find, nub)
import Data.Maybe (fromJust)
import Data.Semigroup (Sum (..))

convert :: String -> Int
convert = sumProds . (liftA2 zip (powers . length) mapChars) . (map toLower)
  where
    charMap = zip (1 : 0 : [2 .. 9]) . nub
    findN m c = fst $ fromJust $ find ((== c) . snd) m
    mapChars = liftA2 (map . findN) charMap id
    powers l = [10 ^ i | i <- [l -1, l -2 .. 0]]
    sumProds = getSum . foldMap (Sum . (uncurry (*)))


-- Exercise 5.8
-----------------------------------------------------------------------------------------------------------
{- 
Many years ago, Roman numbers were defined by only 4 digits: I, V, X, L, 
which represented 1, 5, 10, 50. These were the only digits used. 
The value of a sequence was simply the sum of digits in it. For instance:

IV = VI = 6
IX = XI = 11
XXL = LXX = XLX = 70
It is easy to see that this system is ambiguous, 
and some numbers could be written in many different ways. 
Your goal is to determine how many distinct integers could be represented by exactly n Roman digits grouped together. 
r instance:

solve(1) = 4, because groups of 1 are [I, V, X, L]. 
solve(2) = 10, because the groups of 2 are [II, VI, VV, XI, XV, XX, IL, VL, XL, LL] 
corresponding to [2,6,10,11,15,20,51,55,60,100]. 
solve(3) = 20, because groups of 3 start with [III, IIV, IVV, ...etc]
n <= 10E7
-}
-----------------------------------------------------------------------------------------------------------


import qualified Data.IntSet as Q

solve :: Int -> Int
solve n
  | n >= 100 = 49 * n - 247
  | otherwise = Q.size $ iterate addLetter (Q.singleton 0) !! n where
    addLetter s = foldr Q.union Q.empty $ map (\n -> Q.map (+n) s) [1, 5, 10, 50]


-- Exercise 5.9
-----------------------------------------------------------------------------------------------------------
{- 
Assume we take a number x and perform any one of the following operations:

a) Divide x by 3 (if it is divisible by 3), or
b) Multiply x by 2
After each operation, we write down the result. If we start with 9, we can get a sequence such as:

[9,3,6,12,4,8] -- 9/3=3 -> 3*2=6 -> 6*2=12 -> 12/3=4 -> 4*2=8
You will be given a shuffled sequence of integers 
and your task is to reorder them so that they conform to the above sequence. 
There will always be an answer.

For the above example:
solve([12,3,9,4,6,8]) = [9,3,6,12,4,8].
-}
-----------------------------------------------------------------------------------------------------------


import Control.Arrow ((&&&))
import Data.List (sortOn)
import Data.Ord (Down(..))
import Data.Proxy (Proxy(..))
import GHC.TypeLits (KnownNat, Nat, natVal)

solve :: [Integer] -> [Integer]
solve = sortOn $ Down . ByFactor @3 &&& ByFactor @2

data ByFactor (f :: Nat) n = ByFactor n deriving Show

instance (KnownNat f, Integral n) => Eq (ByFactor f n) where
  x == y = compare x y == EQ

instance (KnownNat f, Integral n) => Ord (ByFactor f n) where
  compare (ByFactor x) (ByFactor y) = go x y
    where f = fromInteger $ natVal $ Proxy @f
          go x y
            | xr /= 0 && yr /= 0 = EQ
            | xr /= 0 && yr == 0 = LT
            | xr == 0 && yr /= 0 = GT
            | otherwise          = go xq yq
            where (xq, xr) = x `quotRem` f
                  (yq, yr) = y `quotRem` f


-- Exercise 5.10
-----------------------------------------------------------------------------------------------------------
{- 
Recursive algorithms can sometimes be optimised with memoisation. 
Often however, the memoisation is tightly coupled with the algorithm, making reuse difficult.

Task
Implement a reusable memoisation function
that, given a function of one argument, returns a memoised function of one argument.

Functions of more than one argument can be memoised by currying the function and memoising it for every argument, 
one at a time. This has easier reusability than having a different memoisation component for every number of arguments.
( This scenario will be tested. )

Performance vs. reusability
Maximum performance is not the goal of this kata; maximum simplicity and reusability is.
If, for any specific application, you need maximum performance, 
you would probably customise your reusable memo function for the job, 
and possibly couple it more tightly, 
thus shifting the balance away from simplicity and reusability more towards performance. 
There's always a balance to be struck, just as there is in resource allocation ( time vs. space, CPU vs. memory ).

Testing
Your function will be tested with naive implementations of recursive staples factorial, 
Ackermann and Fibonacci, and the algorithm for finding a longest common subsequence.
The submit tests are exactly the same as the example tests, only with some random testing thrown in, 
but there are no surprises. If you pass the example tests, you should pass the submit tests, 
and you can read the example tests. ( It is recommended that you do. )

Ackermann and the longest common subsequence implementation will test multiply memoising a curried function.

The datatype of the function argument can be any Enum instance. Any Integrals will be non-negative.
-}
-----------------------------------------------------------------------------------------------------------


import Data.Bits

data Tree a = Tree a (Tree a) (Tree a)
    deriving (Functor)

gen :: Tree Int
gen = Tree 0 (negate <$> tr) tr
    where
        tr = Tree 1 ((* 2) <$> tr) ((+ 1) . (* 2) <$> tr)

find :: Tree a -> Int -> a
find (Tree zero neg pos) n
    | n == 0 = zero
    | n < 0 = go neg (negate n)
    | otherwise = go pos n
    where
        go (Tree one _ _) 1 = one
        go (Tree _ teven todd) k = go next (k `shiftR` 1)
            where
                next = if even k then teven else todd

memo :: (Enum a) => (a -> b) -> (a -> b)
memo fn = (\m -> tr `find` (fromEnum m))
    where
        tr = (fn . toEnum) <$> gen


-- Exercise 5.11
-----------------------------------------------------------------------------------------------------------
{- 
Given a sorted array of distinct integers, 
write a function indexEqualsValue that returns the lowest index for which array[index] == index.
Return -1 if there is no such index.

Your algorithm should be very performant.

[input] array of integers ( with 0-based nonnegative indexing )
[output] integer

Examples:
input: [-8,0,2,5]
output: 2 # since array[2] == 2

input: [-1,0,3,6]
output: -1 # since no index in array satisfies array[index] == index
Random Tests Constraints:
Array length: 10 000

Amount of tests: 1 000

Time limit: 150 ms
-}
-----------------------------------------------------------------------------------------------------------


import Data.Array.Unboxed

indexEqualsValue :: UArray Int Int -> Int
indexEqualsValue xs = uncurry searchIn $ bounds xs
  where searchIn i j
          | i == j && xs ! k == k = k
          | i == j                = -1
          | k <= xs ! k           = searchIn i k
          | otherwise             = searchIn (k + 1) j
          where k = (i + j) `div` 2


-- Exercise 5.12
-----------------------------------------------------------------------------------------------------------
{- 
You have the radius of a circle with the center in point (0,0).

Write a function that calculates the number of points in the circle where (x,y) 
- the cartesian coordinates of the points - are integers.

Example: for radius = 2 the result should be 13.

0 <= radius <= 1000
-}
-----------------------------------------------------------------------------------------------------------


points :: Int -> Int
points r =
  sum
    . map
      ( (+ 1)
          . (* 2)
          . floor
          . circleY r'
          . fromIntegral
      )
    $ [negate r, negate r + 1 .. r]
  where
    r' = fromIntegral r

circleY :: Double -> Double -> Double
circleY r x = sqrt (r * r - x * x)


-- Exercise 5.13
-----------------------------------------------------------------------------------------------------------
{- 
Every possible sum of two digits
Given a long number, return all the possible sum of two digits of it.

For example, 12345: all possible sum of two digits from that number are:

[ 1 + 2, 1 + 3, 1 + 4, 1 + 5, 2 + 3, 2 + 4, 2 + 5, 3 + 4, 3 + 5, 4 + 5 ]
Therefore the result must be:

[ 3, 4, 5, 6, 5, 6, 7, 7, 8, 9 ]
We now interrupt your regularly scheduled programming
Given the result, return the number!

( if there is more than one possibility, just return any one of them )
-}
-----------------------------------------------------------------------------------------------------------


import Data.List (genericLength)

number :: (Integral a) => [a] -> a
number [] = 0
number [1] = 10
number [sum] = 11 * (sum `div` 2) + sum `mod` 2
number sums = foldl ( \ acc v -> 10 * acc + v - n0 ) n0 $ take l sums where
  l = truncate $ sqrt (genericLength sums + genericLength sums + 0.25)
  n0 = (sums !! 0 + sums !! 1 - sums !! l) `div` 2


-- Exercise 5.14
-----------------------------------------------------------------------------------------------------------
{- 
Given an array of numbers, your task is to return a new array 
where each index (new_array[i]) is equal to the product of the original array, 
except for the number at that index (array[i]).

Two things to keep in mind:

Zeroes will be making their way into some of the arrays you are given
O(n^2) solutions will not pass.
Examples:

productSansN [1,2,3,4] -> [24, 12, 8, 6]
productSansN [2,3,4,5] -> [60, 40, 30, 24]
productSansN [1,1,1] -> [1, 1, 1]
productSansN [9,0,-2] -> [0, -18, 0])
productSansN [0,-99,0] -> [0, 0, 0])
productSansN [3,14,9,11,11] -> [15246, 3267, 5082, 4158, 4158])
productSansN [-8,1,5,13,-1] -> [-65, 520, 104, 40, -520])
productSansN [4,7,3,6,2,14,7,5] -> [123480, 70560, 164640, 82320, 246960, 35280, 70560, 98784]
Note: All inputs will be valid arrays of nonzero length.
-}
-----------------------------------------------------------------------------------------------------------


productSansN :: [Integer] -> [Integer]
productSansN xs = go 1 False xs where
  go product True (0:_) = map (const 0) xs
  go product False (0:xs) = go product True xs
  go product zero (x:xs) = go (product*x) zero xs
  go product True [] = map ( \ x -> if x==0 then product else 0 ) xs
  go product False [] = map (product `div`) xs


-- Exercise 5.15
-----------------------------------------------------------------------------------------------------------
{- 
A friend of mine told me privately: "I don't like palindromes". "why not?" 
- I replied. "Because when I want to do some programming challenges, 
I encounter 2 or 3 ones first related with palindromes. I'm fed up" 
- he confess me with anger. I said to myself:"Thankfully, that doesn't happen in Codewars". 
Talking seriously, we have to count the palindrome integers. 
Doing that, perhaps, it will help us to make all the flood of palindrome programming challenges more understandable.

For example all the integers of 1 digit (not including 0) are palindromes, 9 cases. 
We have nine of them with two digits, so the total amount of palidromes below 100 (102) is 18. 
At least, will we be able to calculate the amount of them for a certain number of digits? 
Let's say for 2000 digits? Prepare a code that given the number of digits n, 
may output the amount of palindromes of length equals to n and the total amount of palindromes below 10n.

You will see more examples in the box. Happy coding!!

(You don't know what palindromes are? Investigate, :))
-}
-----------------------------------------------------------------------------------------------------------


countPal :: Int -> (Int,Int)
countPal n = (9 * 10^(h - 1), l n)
  where
    h = (n + 1) `div` 2
    l n | n == 1 = 9
        | even n = 2 * 10^h - 2
        | odd n = l (n-1) - 9*10^(h-1)


-- Exercise 5.16
-----------------------------------------------------------------------------------------------------------
{- 
X and Y are playing a game. A list will be provided which contains n pairs of strings and integers. 
They have to add the integeri to the ASCII values of the stringi characters. 
Then they have to check if any of the new added numbers is prime or not. 
If for any character of the word the added number is prime then the word will be considered as prime word.

Can you help X and Y to find the prime words?

Example:
prime_word({{"Emma",30},{"Liam",30}}) = {1,1};
For the first word "Emma" ASCII values are: 69 109 109 97
After adding 30 the values will be: 99 139 139 127
As 139 is prime number so "Emma" is a Prime Word.
-}
-----------------------------------------------------------------------------------------------------------


import Data.List
import Data.Char

primeWord :: [(String, Int)] -> [Int]
primeWord xs = map (\(s, n) -> if isPrimeShiftedWord n s then 1 else 0) xs

isPrimeShiftedWord :: Int -> String -> Bool
isPrimeShiftedWord n s = case find (isPrimeNum) [ord c + n | c <- s] of
  Just x -> True
  Nothing -> False 

isPrimeNum :: Int -> Bool
isPrimeNum n = null [x | x <- [2..(floor . sqrt $ fromIntegral n)], n `mod` x == 0]


-- Exercise 5.17
-----------------------------------------------------------------------------------------------------------
{- 
Given two words, how many letters do you have to remove from them to make them anagrams?
Example
First word : c od e w ar s (4 letters removed)
Second word : ha c k er r a nk (6 letters removed)
Result : 10
Hints
A word is an anagram of another word if they have the same letters (usually in a different order).
Do not worry about case. All inputs will be lowercase.
-}
-----------------------------------------------------------------------------------------------------------


import qualified Data.HashMap.Strict as HMap
import Data.List

anagramDifference :: String -> String -> Int
anagramDifference xs ys = sum $ map(\c -> if elem c i then abs $ (c1  HMap.! c) - (c2  HMap.! c) else if elem c xs then c1  HMap.! c else c2  HMap.! c) $ nub $ xs++ys
                          where
                          c1 = HMap.fromListWith (+) $ zip xs (repeat 1)
                          c2 = HMap.fromListWith (+) $ zip ys (repeat 1)
                          i = intersect xs ys


-- Exercise 5.18
-----------------------------------------------------------------------------------------------------------
{- 
you will be given a number and your task will be to rearrange the number so that it is divisible by 25, 
but without leading zeros. Return the minimum number of digit moves that are needed to make this possible. 
If impossible, return -1 ( Nothing in Haskell ).

For example:

solve(521) = 3 because:
    a) Move the digit '1' to the front: 521 -> 512 -> 152. The digit '1' is moved two times.
    b) Move '5' to the end: 152 -> 125. The digit '5' is moved one time, so total movement = 3.
Of all the ways to accomplish this, the least digit moves = 3.

solve(100) = 0. Number already divisible by 25.
solve(1) = -1. Not possible to make number divisible by 25.

solve(0) is not tested.
-}
-----------------------------------------------------------------------------------------------------------


import Data.Maybe
import Data.List

solve :: Integer -> Maybe Int
solve n | mod n 25 == 0 = Just 0
        | null x = Nothing
        | otherwise = Just $ minimum x
          where
          h = reverse $ show n
          x = map fromJust $ filter(/=Nothing) [solve50 h, solve00 h, solve5 h]

remove :: Char -> String -> String
remove c s = (take i s) ++ (drop (i+1) s)
             where i = length $ takeWhile(/=c) s

solve50 :: String -> Maybe Int
solve50 n | (intersect "05" n) /= "05" = Nothing
          | otherwise = Just $ ind1 + ind2 + (nonZero $ remove '0' $ remove '5' n) + dif
            where
            ind1 = length $ takeWhile(/='0') n
            ind2 = length $ takeWhile(/='5') n
            dif | ind1 < ind2 = -1
                | otherwise   = 0
           
solve00 :: String -> Maybe Int
solve00 n | (length $ filter(=='0') n) < 2 = Nothing
          | otherwise = Just $ (length $ takeWhile(/='0') n) + (length $ takeWhile(/='0') $ remove '0' n)


solve5 :: String -> Maybe Int
solve5 n | (notElem '5' n) || (null $ intersect "72" n) = Nothing
         | otherwise = Just $ ind1 + ind2 + dif + (nonZero $ map fst $ filter(\(_,i) -> notElem i [ind1, ind2]) $ zip n [0..])
          where
          ind1 = head $ filter(\i -> elem (n!!i) "725") [0..]
          (ind2, dif) | (n!!ind1) == '5' = (head $ filter(\i -> elem (n!!i) "72") [0..], -1)
                      | otherwise        = (length $ takeWhile(/='5') n, 0)
          
nonZero :: String -> Int
nonZero n | i == (length n) = 0
          | otherwise = i
            where 
            e = reverse n
            i = length $ takeWhile(=='0') e


-- Exercise 5.19
-----------------------------------------------------------------------------------------------------------
{- 
you will be given an integer n and your task will be to return the largest integer that is <= n 
and has the highest digit sum.

For example:

solve(100) = 99. Digit Sum for 99 = 9 + 9 = 18. No other number <= 100 has a higher digit sum.
solve(10) = 9
solve(48) = 48. Note that 39 is also an option, but 48 is larger.
Input range is 0 < n < 1e11
-}
-----------------------------------------------------------------------------------------------------------


import Data.List
import Data.Maybe
import Data.Char

solve :: Int -> Int
solve n | i == l || f e <= f s = n
        | otherwise            = read e
          where
          s = show n
          l = length s
          h = [head s] ++ takeWhile(=='9') (tail s)
          i = length h
          e = (show $ (read h) - 1) ++ (replicate (l-i) '9')
          f = sum . map digitToInt


-- Exercise 5.20
-----------------------------------------------------------------------------------------------------------
{- 
Consider the sequence S(n, z) = (1 - z)(z + z**2 + z**3 + ... + z**n) 
where z is a complex number and n a positive integer (n > 0).

When n goes to infinity and z has a correct value (ie z is in its domain of convergence D), 
S(n, z) goes to a finite limit lim depending on z.

Experiment with S(n, z) to guess the domain of convergence Dof S and lim value when z is in D.

Then determine the smallest integer n such that abs(S(n, z) - lim) < eps 
where eps is a given small real number and abs(Z) is the modulus or norm of the complex number Z.

Call f the function f(z, eps) which returns n. 
If z is such that S(n, z) has no finite limit (when z is outside of D) f will return -1.

Examples:
I is a complex number such as I * I = -1 (sometimes written i or j).

f(0.3 + 0.5 * I, 1e-4) returns 17

f(30 + 5 * I, 1e-4) returns -1

Remark:
For languages that don't have complex numbers or "easy" complex numbers, 
a complex number z is represented by two real numbers x (real part) and y (imaginary part).

f(0.3, 0.5, 1e-4) returns 17

f(30, 5, 1e-4) returns -1

Note:
You pass the tests if abs(actual - exoected) <= 1
-}
-----------------------------------------------------------------------------------------------------------


import Data.Complex

hypot :: Double -> Double -> Double
hypot a b = sqrt (a * a + b * b)

f :: Complex Double -> Double -> Int
f z eps = let ans = log eps / log (hypot (realPart z) (imagPart z)) in 
  if ans < 0 then -1 else (floor ans)
