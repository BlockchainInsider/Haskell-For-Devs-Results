module Exercise04 where


-- Exercise 4.1
-----------------------------------------------------------------------------------------------------------
{- 
you're given the following function

findX :: Int -> Int
findX 0 = 0
findX n = sum $ map ( \ i -> findX (i-1) + 3 * i ) [1..n]
Since this computation is exponential, it gets very slow quickly as n increases, 
your task is to optimize the function so it works well for large numbers.

findX 2 -- -> 12
findX 3 -- -> 33
findX 5 -- -> 171
The result gets pretty large even for small inputs, so you should return the result % (109+7)

Input Range
1 <= n <= 10^6
-}
-----------------------------------------------------------------------------------------------------------


powMod :: Integer -> Integer -> Integer -> Integer
powMod _ 0 m = 1
powMod b e m | even e = powMod (b * b `mod` m) (e `div` 2) m
             | otherwise = b * powMod b (e - 1) m `mod` m

findX :: Int -> Int
findX 0 = 0
findX n = fromIntegral $ 3 * (powMod 2 (n' + 1) m - n' - 2) `mod` m
  where m = 10 ^ 9 + 7
        n' = fromIntegral n :: Integer


-- Exercise 4.2
-----------------------------------------------------------------------------------------------------------
{- 
write a function that will receive an array of strings as its single argument, 
then the strings are each processed and sorted (in desending order) 
based on the length of the single longest sub-string of contiguous vowels ( aeiouAEIOU ) 
that may be contained within the string. 
The strings may contain letters, numbers, special characters, uppercase, lowercase, whitespace, 
and there may be (often will be) multiple sub-strings of contiguous vowels. 
We are only interested in the single longest sub-string of vowels within each string, in the input array.

Example:

str1 = "what a beautiful day today"
str2 = "it's okay, but very breezy"
When the strings are sorted, str1 will be first as its longest sub-string of contiguous vowels "eau" 
is of length 3, while str2 has as its longest sub-string of contiguous vowels "ee", which is of length 2.

If two or more strings in the array have maximum sub-strings of the same length, 
then the strings should remain in the order in which they were found in the orginal array.
-}
-----------------------------------------------------------------------------------------------------------


import Data.Char
import Data.List
import Data.Ord
import Data.Function

sortByVowels :: [String] -> [String]
sortByVowels = sortBy (\a -> neg . comparing maxVoewlChainLength a)

neg GT = LT; neg LT = GT; neg e = e

maxVoewlChainLength str = if null vowelGroups then 0 else maximum $ map length vowelGroups
  where vowelGroups = filter (isVowel . head) $ groupBy ((==) `on` isVowel) str

isVowel = flip elem "aaieou" . toLower


-- Exercise 4.3
-----------------------------------------------------------------------------------------------------------
{-
The regular paperfolding sequence, also known as the dragon curve sequence, 
is an infinite automatic sequence of 0s and 1s 
defined as the limit of inserting an alternating sequence of 1s and 0s around 
and between the terms of the previous sequence:

https://en.wikipedia.org/wiki/Regular_paperfolding_sequence

1

1 1 0

1 1 0 1 1 0 0

1 1 0 1 1 0 0 1 1 1 0 0 1 0 0

...

Note how each intermediate sequence is a prefix of the next.

Define a list paperFold that contains the values of this sequence:

1 1 0 1 1 0 0 1 1 1 0 0 1 0 0 1 1 1 0 1 1 0 0 0 1 1 0 0 1 0 0 ...

It will be tested for up to 1 000 000 values.
-}
-----------------------------------------------------------------------------------------------------------



f :: Int -> Int
f n
  | n `mod` 2 == 0 = f $ n `div` 2
  | n `mod` 4 == 1 = 1
  | otherwise = 0

paperFold = map f [1..]


-- Exercise 4.4
-----------------------------------------------------------------------------------------------------------
{- 
The Takeuchi function is defined as:

t :: Int -> Int -> Int -> Int
t x y z = if x <= y then
            y
          else
            t (t (x-1) y z) (t (y-1) z x) (t (z-1) x y)

Let us define function U as:

u :: Int -> Int
u n = t n 0 (n+1)

Let E(n) be the number of times the else clause in the T function is invoked when computing U(n) 
(no memoization or laziness). We get:

e  5 =     223 -- for n = 5, the else clause is invoked 223 times in the t function
e 10 = 1029803

You will be given a number n and your task is to return the digital sum of E(n):

solve  5 =   7 -- e  5 =     223 and the sum of digits in 223 = 7 because 223 -> 2 + 2 + 3 = 7
solve 10 =  23 -- e 10 = 1029803 and the sum of digits in 1029803 is 23
n will not exceed 165. More examples in test cases. Good luck!
-}
-----------------------------------------------------------------------------------------------------------


import Data.Char (digitToInt)

type Cost = Integer
type Costly a = (Cost, a) -- annotates a result with a cost

digitSum :: Cost -> Int
digitSum = sum . map digitToInt . show

cost :: Costly a -> Cost
cost = fst -- gets out just the cost annotation

t :: Int -> Int -> Int -> Costly Int
t = memo $ \ x -> memo $ \ y -> memo $ \ z -> -- look through the memo and it's just `\ x y z ->`, written as `\ x -> \ y -> \z ->` so I can memoise every variable singly
-- t x y z = -- normally you'd write it like this
      if x <= y then
        (0,y) -- annotates this result with cost 0
      else
        let (ix,xx) = t (x-1) y z
            (iy,yy) = t (y-1) z x
            (iz,zz) = t (z-1) x y
            (it,tt) = t xx yy zz
        in  ( ix + iy + iz + it + 1, tt ) -- computes result and annotates with total cost

solve :: Int -> Int
solve n = digitSum $ cost $ t n 0 (n+1)

-- memo credit: dramforever

-- this memo can handle negative numbers

data Tree a = Tree a (Tree a) (Tree a) deriving (Functor)

gen :: Tree Int
gen = Tree 0 (negate <$> tree) tree where
  tree = Tree 1 ((* 2) <$> tree) ((+ 1) . (* 2) <$> tree)

find :: Tree a -> Int -> a
find (Tree zero _ _) 0 = zero
find (Tree _ neg pos) n | n < 0 = go neg (negate n)
                        | otherwise = go pos n
                        where go (Tree one _ _) 1 = one
                              go (Tree _ teven todd) k | even k    = go teven (k `div` 2)
                                                       | otherwise = go todd (k `div` 2)

memo :: (Enum a) => (a -> b) -> (a -> b)
memo fn = find (fn . toEnum <$> gen) . fromEnum -- if it only needed to handle positive numbers, and you didn't mind O(n) access instead of O(log n), this could be
-- memo fn x = (map (fn . toEnum) [0..]) !! (fromEnum x)


-- Exercise 4.5
-----------------------------------------------------------------------------------------------------------
{- 
In an infinite array with two rows, the numbers in the top row are denoted

. . . , A[−2], A[−1], A[0], A[1], A[2], . . .

and the numbers in the bottom row are denoted

. . . , B[−2], B[−1], B[0], B[1], B[2], . . .

For each integer k, the entry A[k] is directly above the entry B[k] in the array, as shown:

... | A[-2] | A[-1] | A[0]  | A[1]  | A[2]  | ...
... | B[-2] | B[-1] | B[0]  | B[1]  | B[2]  | ...
For each integer k, A[k] is the average of the entry to its left, 
the entry to its right, and the entry below it; 
similarly, each entry B[k] is the average of the entry to its left, 
the entry to its right, and the entry above it.

Given A[0], A[1], A[2] and A[3], determine the value of A[n]. (Where range of n is -1000<n<1000)

Test Cases are called as an array of ([A[0], A[1], A[2], A[3]], n)

Hint: Calculate B[k]

FOR JS Node v10.x -> Inputs and Outputs in BigInt!

Adapted from 2018 Euclid Mathematics Contest. 
https://www.cemc.uwaterloo.ca/contests/past_contests/2018/2018EuclidContest.pdf
-}
-----------------------------------------------------------------------------------------------------------


findA :: [Integer] -> Int -> Integer
findA [a0,_,_,_] 0 = a0
findA [_,a1,_,_] 1 = a1
findA [_,_,a2,_] 2 = a2
findA [_,_,_,a3] 3 = a3
findA as n | n<0 = findA (reverse as) (3-n)
           | otherwise = findA [a1,a2,a3,a4] (n-1)
           where [a0,a1,a2,a3] = as
                 a4 = 6 * a3 - 10 * a2 + 6 * a1 - a0


-- Exercise 4.6
-----------------------------------------------------------------------------------------------------------
{- 
A strongness of an even number is the number of times we can successively divide by 2 
until we reach an odd number starting with an even number n.

For example, if n = 12, then

12 / 2 = 6
6 / 2 = 3
So we divided successively 2 times and we reached 3, so the strongness of 12 is 2.

If n = 16 then

16 / 2 = 8
8 / 2 = 4
4 / 2 = 2
2 / 2 = 1
we divided successively 4 times and we reached 1, so the strongness of 16 is 4

Task
Given a closed interval [n, m], return the even number that is the strongest in the interval. 
If multiple solutions exist return the smallest strongest even number.

Note that programs must run within the allotted server time; a naive solution will probably time out.

Constraints
1 <= n < m <= INT_MAX

Examples
[1, 2]    -->   2  # 1 has strongness 0, 2 has strongness 1
[5, 10]   -->   8  # 5, 7, 9 have strongness 0; 6, 10 have strongness 1; 8 has strongness 3
[48, 56]  -->  48
-}
-----------------------------------------------------------------------------------------------------------


strongestEven :: Int -> Int -> Int
strongestEven n m | h > (l n) = 2 ^ h
                  | e == k = k
                  | otherwise = 2 * strongestEven (div k 2) (div e 2)
                    where
                    l x = floor $ logBase 2 (fromIntegral x)
                    h = l m
                    k = n + (mod n 2)
                    e = m - (mod m 2)


-- Exercise 4.7
-----------------------------------------------------------------------------------------------------------
{-
Let us consider this example (array written in general format):

ls = [0, 1, 3, 6, 10]

Its following parts:

ls = [0, 1, 3, 6, 10]
ls = [1, 3, 6, 10]
ls = [3, 6, 10]
ls = [6, 10]
ls = [10]
ls = []
The corresponding sums are (put together in a list): [20, 20, 19, 16, 10, 0]

The function parts_sums (or its variants in other languages) 
will take as parameter a list ls and return a list of the sums of its parts as defined above.

Other Examples:
ls = [1, 2, 3, 4, 5, 6] 
parts_sums(ls) -> [21, 20, 18, 15, 11, 6, 0]

ls = [744125, 935, 407, 454, 430, 90, 144, 6710213, 889, 810, 2579358]
parts_sums(ls) -> [10037855, 9293730, 9292795, 9292388, 9291934, 9291504, 9291414, 9291270, 2581057, 2580168, 2579358, 0]
Notes
Take a look at performance: some lists have thousands of elements.
Please ask before translating.
-}
-----------------------------------------------------------------------------------------------------------


partsSum :: [Integer] -> [Integer]
partsSum arr = helper arr (sum arr)

helper :: [Integer] -> Integer -> [Integer]
helper [] _ = [0]
helper (x:xs) summe = [summe] ++ helper xs (summe-x)


-- Exercise 4.8
-----------------------------------------------------------------------------------------------------------
{- 
Given a number, num, return the shortest amount of steps it would take from 1, to land exactly on that number.

Description:
A step is defined as either:

Adding 1 to the number: num += 1
Doubling the number: num *= 2
You will always start from the number 1 and you will have to return the shortest count of steps 
it would take to land exactly on that number.

1 <= num <= 10000

Examples:

num == 3 would return 2 steps:

1 -- +1 --> 2:        1 step
2 -- +1 --> 3:        2 steps

2 steps
num == 12 would return 4 steps:

1 -- +1 --> 2:        1 step
2 -- +1 --> 3:        2 steps
3 -- x2 --> 6:        3 steps
6 -- x2 --> 12:       4 steps

4 steps
num == 16 would return 4 steps:

1 -- +1 --> 2:        1 step
2 -- x2 --> 4:        2 steps
4 -- x2 --> 8:        3 steps
8 -- x2 --> 16:       4 steps

4 steps
-}
-----------------------------------------------------------------------------------------------------------


num n m =
  if n==1 then m
  else
    if (mod n 2) == 0 then num (div n 2) (m+1)
    else num (n-1) (m+1)


steps :: Int -> Int
steps n = num n 0



-- Exercise 4.9
-----------------------------------------------------------------------------------------------------------
{- 
Number pyramid is a recursive structure where each next row is constructed 
by adding adjacent values of the current row. For example:

Row 1     [1     2     3     4]
Row 2        [3     5     7]
Row 3           [8    12]
Row 4             [20]
Task
Given the first row of the number pyramid, find the value stored in its last row.

Performance tests
Number of tests: 5
List size: 10,000
-}
-----------------------------------------------------------------------------------------------------------


line k n p
  | k == n = []
  | otherwise = calc : line (k+1) n calc
  where calc = toInteger ( p * (n-k) `div` (k+1) )

pascal n = 1 : line 0 n 1

reducePyramid :: [Integer] -> Integer
reducePyramid [1] = 1
reducePyramid xs = sum $ zipWith (*) xs $ pascal (toInteger $ (length xs) - 1)


-- Exercise 4.10
-----------------------------------------------------------------------------------------------------------
{- 
Upside-Down Pyramid Addition is the process of taking a list of numbers 
and consecutively adding them together until you reach one number.

When given the numbers 2, 1, 1 the following process occurs:

 2   1   1
   3   2 
     5
This ends in the number 5.

YOUR TASK
Given the right side of an Upside-Down Pyramid (Ascending), write a function that will return the original list.

EXAMPLE
reversePyramid [5, 2, 1] -> [2, 1, 1]
NOTE: The Upside-Down Pyramid will never be empty and will always consist of positive integers ONLY.
-}
-----------------------------------------------------------------------------------------------------------


reversePyramid :: [Int] -> [Int]
reversePyramid = reversePyramid' [] . reverse where
    reversePyramid' rs [] = rs
    reversePyramid' rs xs = reversePyramid' (head xs : rs) $ zipWith (-) (tail xs) xs


-- Exercise 4.11
-----------------------------------------------------------------------------------------------------------
{- 
If you're old enough, you might remember buying your first mobile phone, one of the old ones with no touchscreen, 
and sending your first text message with excitement in your eyes. 
Maybe you still have one lying in a drawer somewhere.

Let's try to remember the good old days and what it was like to send text messages with such devices. 
A lot of them had different layouts, most notably for special characters and spaces, 
so for simplicity we'll be using a fictional model with 3x4 key layout shown below:

-------------------------
|   1   |   2   |   3   |  <-- hold a key to type a number
|  .,?! |  abc  |  def  |  <-- press a key to type a letter
-------------------------
|   4   |   5   |   6   |  <-- Top row
|  ghi  |  jkl  |  mno  |  <-- Bottom row
-------------------------
|   7   |   8   |   9   |
|  pqrs |  tuv  |  wxyz |
-------------------------
|   *   |   0   |   #   |  <-- hold for *, 0 or #
|  '-+= | space |  case |  <-- press # to switch between upper/lower case
-------------------------
The task
You got your thumb ready to go, so you'll receive a message and your job is to figure out 
which keys you need to press to output the given message with the lowest number of clicks possible. 
Return the result as a string of key inputs from top row (refer to diagram above).

Take your time to study the rules below.

How it works
Result
Output string contains inputs that are shown at the top row of a key's layout. (0-9*#)

Typing letters
To type letters, press a button repeatedly to cycle through the possible characters (bottom row of a key's layout). 
Pressing is represented by key's top row element repeated n times, 
where n is the position of character on that particular key. Examples:

2 => 'a', 9999 => 'z', 111 => '?', *** => '+'
Typing numbers
To type numbers 0-9 and special characters *# - hold that key. 
Holding is represented by a number, followed by a dash. Examples:

3- => '3', 5-5-5- => '555'
Uppercase / Lowercase
Initially the case is lowercase. To toggle between lowercase and uppercase letters, use the # symbol. 
Case switching should only be considered when next character is alphabetic (a-z). Examples:

#2#9999 => 'Az' (remember, it's a toggle)
27-#2255 => 'a7BK' (do not switch before '7')
Waiting for next character
If you have 2 or more characters in a sequence that reside on the same button (refer to layout, bottom row), 
you have to wait before pressing the same button again. 
Waiting is represented by adding a space between 2 (or more) such characters. Example:

44 444 44 444 => 'hihi'
Exceptions: No need to wait after holding any key, 
even if next character resides on same button (4-4 => '4g'), 
or if there's a case switch between 2 characters on same button (#5#55 => 'Jk').

Example
To put it all together, let's go over an example. Say you want to type this message - 'Def Con 1!':

Switch to uppercase with # and press 3 (#3 => D) (input is now in uppercase mode)
Switch to lowercase and press 3 twice (#33 => e). (Note that there is no waiting because of case switching)
Next character f is on button 3 again, and has the same case (lowercase input and lowercase character), 
so you have to wait to type again (' 333' => f).
In a similar manner type the second word (space is located on number 0). 0#222#666 660 => ' Con '
Finish off by holding 1 as 1- and typing ! as 1111 ('1-1111' = 1!). 
Note that after holding a key you don't need to wait to press another key.
Result:
sendMessage("Def Con 1!") => "#3#33 3330#222#666 6601-1111" 

More examples are provided in sample test suite.

All inputs will be valid strings and only consist of characters from the key layout table.
-}
-----------------------------------------------------------------------------------------------------------


import Data.List (foldl')
import Data.Char (isUpper, isLetter, toUpper)
import qualified Data.Map.Strict as M
import Data.Bifunctor (first)


sendMessage :: String -> String
sendMessage = concat . reverse . fst . foldl' f ([], False)
  where
    f (acc, u) c =
      let
        s = charMap M.! c
        u' = if isLetter c then isUpper c else u
        switchCase = u' /= u
        insertPause (x : _) = last x == head s
        insertPause _ = False
      in
        if switchCase
          then (s : "#" : acc, u')
          else if insertPause acc
            then (s : " " : acc, u')
            else (s : acc, u')

charMap = M.fromList $ mappings ++ map (first toUpper) mappings
  where
    layout =
      [('1', ".,?!"), ('2', "abc"), ('3', "def"), ('4', "ghi"), ('5', "jkl"),
      ('6', "mno"), ('7', "pqrs"), ('8', "tuv"), ('9', "wxyz"), ('0', " "),
      ('*', "'-+="), ('#', "")]
    mappings = concatMap (\(b, ls) -> (b, b : "-") : zipWith (\i l -> (l, replicate i b)) [1..] ls) layout


-- Exercise 4.12
-----------------------------------------------------------------------------------------------------------
{- 
Write a function f so that f x gives a list of one hundred xs.

Code Length
28 chars or shorter (including the module line).

Protip
If you're stuck at 30 chars, giving up is a very good option.
-}
-----------------------------------------------------------------------------------------------------------


f=(<$[0..99])


-- Exercise 4.13
-----------------------------------------------------------------------------------------------------------
{- 
Your task is to guess a number between 1 and 100 using only 7 tests.

You are given a function greaterThan :: Monad m => Int -> m Bool.
-}
-----------------------------------------------------------------------------------------------------------


guess :: Monad m => (Int -> m Bool) -> m Int
guess gt = guessBinary gt 0 100
guessBinary :: Monad m => (Int -> m Bool) -> Int -> Int -> m Int
guessBinary gt begin end | begin + 1 == end = return end
guessBinary gt begin end  =
    let mid = (begin + end) `div` 2
        guess = gt mid
    in do
          indicator <- guess
          if indicator
          then guessBinary gt mid end
          else guessBinary gt begin mid


-- Exercise 4.14
-----------------------------------------------------------------------------------------------------------
{- 
The Bifid cipher makes use of a 5 x 5 Polybius square. 
The letters of a message are encoded via the coordinates of that letter in the square.

     1 2 3 4 5
   1 A B C D E
   2 F G H I K
   3 L M N O P
   4 Q R S T U
   5 V W X Y Z
In the square all the letters of the alphabet except 'J' are present. 
Each letter appears only one time. Each 'J' in input must be changed to 'I'. 
White spaces are ignored, they shall be removed from the key and from the message.

The square can be changed by using a secret key. 
If the secret key is "CODEWARS" the grid is adapted by placing the unique letters 
of the key in front of the alphabet. 
The key "CODE WARS" gives the same result, because the white space has to be removed. 
It leads to the following square.

  1 2 3 4 5
1 C O D E W
2 A R S B F 
3 G H I K L
4 M N P Q T
5 U V X Y Z 
To encode a message the coordinates of each letter are written in a column below the corresponding letter.

With key "CODEWARS" the message "WARRIOR" is written down as follows:

Message: WARRIOR
Row:     1222312
Column:  5122322
The encoding takes place by reading horizontally and finding the corresponding letters.

12 22 31 25 12 23 22 -> ORGFOSR

Of course the decoding is done reversely.

Only capital letters and white spaces are used.

Your challenge is to write two functions encodeBifid and decodeBifid.
-}
-----------------------------------------------------------------------------------------------------------


import Data.Char (intToDigit)
import Data.List (elemIndex, nub)
import Data.Maybe (fromJust)
import Data.List.Split

alpha = "ABCDEFGHIKLMNOPQRSTUVWXYZ"

getMatrix :: String -> String
getMatrix xs = (nub xs) ++ tail
               where tail = filter (\x -> x `notElem` xs) alpha


getPos :: String -> Char -> (Int, Int)
getPos matrix c = (row + 1, col + 1)
                  where pos = fromJust $ elemIndex c matrix
                        row = pos `div` 5
                        col = pos `mod` 5
                         

clean :: String -> String
clean = filter (\x -> x `elem` alpha) . map (\x -> if x == 'J' then 'I' else x)


getEncoding :: [Int] -> String -> String
getEncoding [] _ = []
getEncoding (i:j:xs) matrix = [matrix!!pos] ++ getEncoding xs matrix
                             where pos = ((i-1) * 5) + (j-1)
          


encodeBifid :: String -> String -> String
encodeBifid key message = getEncoding t matrix
                          where [k, m] = [clean key, clean message]
                                matrix = getMatrix k
                                p = map (\c -> getPos matrix c) m
                                t = concat [map fst p, map snd p]
                     

getDecoding :: [(Int, Int)] -> String -> String
getDecoding [] _ = []
getDecoding ((i, j): xs) matrix = [matrix!!pos] ++ getDecoding xs matrix
                                  where pos = ((i-1) * 5) + (j-1)

decodeBifid :: String -> String -> String
decodeBifid key message =  getDecoding z matrix
                   where [k, m] = [clean key, clean message]
                         matrix = getMatrix k
                         p = map (\c -> getPos matrix c) m
                         flat = [ n |(a,b)<-p,n <-[a,b]]
                         (a, b) = splitAt ((length flat + 1) `div` 2) flat
                         z = zip a b


-- Exercise 4.15
-----------------------------------------------------------------------------------------------------------
{- 
A pair of numbers has a unique LCM but a single number can be the LCM of more than one possible pairs. 
For example 12 is the LCM of (1, 12), (2, 12), (3,4) etc. 
For a given positive integer N, the number of different integer pairs with LCM 
is equal to N can be called the LCM cardinality of that number N. 
In this kata your job is to find out the LCM cardinality of a number.
-}
-----------------------------------------------------------------------------------------------------------


import Data.List

lcmCardinality :: Int -> Int
lcmCardinality = (`div` 2) . succ . product . map (succ . (* 2) . length) . group . factorize

factorize :: Int -> [Int]
factorize = factorize' 2 where
    factorize' _ 1 = []
    factorize' d n
        | d * d > n = [n]
        | n `mod` d == 0 = d : factorize' d (n `div` d)
        | otherwise = factorize' (d + 1) n


-- Exercise 4.16
-----------------------------------------------------------------------------------------------------------
{- 
Given time in 24-hour format, convert it to words.

For example:
13:00 = one o'clock 
13:09 = nine minutes past one 
13:15 = quarter past one 
13:29 = twenty nine minutes past one
13:30 = half past one 
13:31 = twenty nine minutes to two
13:45 = quarter to two 
00:48 = twelve minutes to one
00:08 = eight minutes past midnight
12:00 = twelve o'clock
00:00 = midnight

Note: If minutes == 0, use 'o'clock'. If minutes <= 30, use 'past', and for minutes > 30, use 'to'. 
More examples in test cases. Good luck!
-}
-----------------------------------------------------------------------------------------------------------


solve :: String -> String  
solve = tell . parse  
 where
    parse :: String -> (Int, Int)
    parse s = (read $ take 2 s, read $ drop 3 s)
    tellNum :: Int -> String
    tellNum 1 = "one"
    tellNum 2 = "two"
    tellNum 3 = "three"
    tellNum 4 = "four"
    tellNum 5 = "five"
    tellNum 6 = "six"
    tellNum 7 = "seven"
    tellNum 8 = "eight"
    tellNum 9 = "nine"
    tellNum 10 = "ten"
    tellNum 11 = "eleven"
    tellNum 12 = "twelve"
    tellNum 13 = "thirteen"
    tellNum 14 = "fourteen"
    tellNum 16 = "sixteen"
    tellNum 17 = "seventeen"
    tellNum 18 = "eighteen"
    tellNum 19 = "nineteen"
    tellNum 20 = "twenty"
    tellNum 21 = "twenty one"
    tellNum 22 = "twenty two"
    tellNum 23 = "twenty three"
    tellNum 24 = "twenty four"
    tellNum 25 = "twenty five"
    tellNum 26 = "twenty six"
    tellNum 27 = "twenty seven"
    tellNum 28 = "twenty eight"
    tellNum 29 = "twenty nine"
    tellHour :: Int -> String
    tellHour 0 = "midnight"
    tellHour h
      | h <= 12 = tellNum h
      | otherwise = tellHour (h `mod` 12)
    tell :: (Int, Int) -> String
    tell (0, 0) = "midnight"
    tell (h, 0) = (tellHour h) ++ " o'clock"
    tell (h, 15) = "quarter past " ++ (tellHour h)
    tell (h, 30) = "half past " ++ (tellHour h)
    tell (h, 45) = "quarter to " ++ (tellHour (h + 1))
    tell (h, 1) = "one minute past " ++ (tellHour h)
    tell (h, 59) = "one minute to " ++ (tellHour (h + 1))
    tell (h, m)
      | m < 30 = (tellNum m) ++ " minutes past " ++ (tellHour h)
      | otherwise = (tellNum (60 - m)) ++ " minutes to " ++ (tellHour (h + 1))


-- Exercise 4.17
-----------------------------------------------------------------------------------------------------------
{- 
You will be given an array of strings. 
The words in the array should mesh together where one or more letters 
at the end of one word will have the same letters (in the same order) as the next word in the array. 
But, there are times when all the words won't mesh.

Examples of meshed words:

"apply" and "plywood"

"apple" and "each"

"behemoth" and "mother"

Examples of words that do not mesh:

"apply" and "playground"

"apple" and "peggy"

"behemoth" and "mathematics"

If all the words in the given array mesh together, 
then your code should return the meshed letters in a string. 
You should return the longest common substring. 
You won't know how many letters the meshed words have in common, but it will be at least one.

If any pair of consecutive words fails to mesh, your code should return "failed to mesh".

Input: An array of strings. There will always be at least two words in the input array.

Output: Either a string of letters that mesh the words together or the string "failed to mesh".

Examples
#1:

["allow", "lowering", "ringmaster", "terror"] --> "lowringter"
because:

the letters "low" in the first two words mesh together
the letters "ring" in the second and third word mesh together
the letters "ter" in the third and fourth words mesh together.
#2:

["kingdom", "dominator", "notorious", "usual", "allegory"] --> "failed to mesh"
Although the words "dominator" and "notorious" share letters in the same order, 
the last letters of the first word don't mesh with the first letters of the second word.
-}
-----------------------------------------------------------------------------------------------------------



import Data.List  (isPrefixOf, tails)
import Data.Maybe (listToMaybe)

wordMesh :: [String] -> Maybe String
wordMesh = fmap concat . sequenceA . (zipWith mesh <*> tail)
  where
    mesh x y = listToMaybe . filter (`isPrefixOf` y) . init . tails $ x


-- Exercise 4.18
-----------------------------------------------------------------------------------------------------------
{- 
Drop n elements from the end of the list xs.

notes
For values of n <= 0, this should return xs; for values of n >= length xs, 
this should return [] ( same as drop does ).
For infinite xs, this should just return xs. Yes, xs can be infinite.
-}
-----------------------------------------------------------------------------------------------------------


dropFromEnd :: Int -> [a] -> [a]
dropFromEnd n xs = dropWithRest xs $ drop n xs
  where
  dropWithRest [] _ = []
  dropWithRest _ [] = []
  dropWithRest (x:xs) (r:rs) = x : dropWithRest xs rs


-- Exercise 4.19
-----------------------------------------------------------------------------------------------------------
{- 
Mr. Mxyzinjin is the greatest kata author in the world. He has many fans around the world.

Today, he received letters from x fans. Fans want to get his autographed photos. But, he has only n photos.

He plans to select n people from these fans and send out his photos.

According to the order of the letters, he gave each fan a ID number(let's say, id1), 
from 1 to x, where x is the total number of fans.

According to his initial impression, he gave each fan an initial weight.

Reordering the fans according to the initial weights, from big to small. 
If two fans have the same initial weight, the fan who has smaller id1 value is in front.

According to the order of current sequence, he gave each fan a new ID number(let's say, id2), 
from 1 to x, where x is the total number of fans.

He divided the fans into 10 kinds, according to the value of id2 mod 10. 
For example, 1, 11 and 21 are the same kind, 2, 12,22 are the same kind, etc.

Each kind of fans will plus a special weight.

Reordering the fans according to the current weights, from big to small. 
If two fans have the same weight, the fan who has smaller id1 value is in front. (Note, id1, not id2)

Finally, pick n fans from big weight to small weight, and send out his photos.

You are given the initial weights, special weight, and n. 
Your task is to sort the data and select n fans, return their id (id1).

Input
initWeights. An integer array. Each element is the initial weight of each fan, 
according to the order of the letters. 3<= initWeights.length <= 10000

specWeights. An integer array. It always contains 10 elements, 
and each element represents a special weight of a kind of fans. 
1st element for kind 1, 2nd element for kind2, and so on..

n. A positive integer. The number of selected fans. 1 <= n <= 200 and n <= initWeights.length

Output
An array of n fans' ids(id1), according to the order of final result after sort operations.

Example
For initWeights = [1,2,3,4], specWeights = [1,1,1,100,0,0,0,0,0,0], and n = 3,

The output should be [1,4,3]

           id1:   1  2  3  4
initial weight:   1  2  3  4

sort by weight:

           id1:   4  3  2  1
initial weight:   4  3  2  1

plus special weight:

           id1:   4  3  2  1
initial weight:   4  3  2  1
           id2:   1  2  3  4
special weight:   1  1  1  100 0 0 0 0 0 0
current weight:   5  4  3  101

sort by weight:

           id1:   1   4  3  2
current weight:   101 5  4  3

select 3 fans:

[1,4,3]
See more examples in the sample tests.

Note
All indexs in description are 1-based.

3 fixed testcases

100 random testcases, testing for correctness of solution

All inputs are valid.

If my reference solution gives the wrong result in the random tests, please let me know(post an issue).
-}
-----------------------------------------------------------------------------------------------------------


import Data.List


initIds = [1..]

compareIdsAndWeights :: (Int, Int) -> (Int, Int) -> Ordering
compareIdsAndWeights (i1, w1) (i2, w2) =
  if comparedWeights == EQ
  then antiComparedIds
  else comparedWeights
  
  where antiComparedIds = compare i1 i2
        comparedWeights = flip compare w1 w2


who :: [Int] -> [Int] -> Int -> [Int]
who initWeights specWeights10 n =  take n sortedIds  
  where
    specWeights = cycle specWeights10
    sortedIds = map fst sortedIdsAndInitWeightsPlusSpec
  
    sortedIdsAndInitWeightsPlusSpec = 
        sortBy compareIdsAndWeights
          $ zip sortedIds1ByInitWeights 
                sortedInitWeightsPlusSpec
  
    sortedIds1ByInitWeights =
              map fst sortedIds1AndInitWeights
    
    sortedInitWeights = map snd sortedIds1AndInitWeights
    
    sortedInitWeightsPlusSpec = 
              zipWith (+) specWeights sortedInitWeights
    
    sortedIds1AndInitWeights =
        sortBy compareIdsAndWeights
          $ zip initIds initWeights


-- Exercise 4.20
-----------------------------------------------------------------------------------------------------------
{- 
Some languages like Chinese, Japanese, and Thai do not have spaces between words. 
However, most natural languages processing tasks like part-of-speech tagging require texts that have segmented words. 
A simple and reasonably effective algorithm to segment a sentence into its component words is called "MaxMatch".

MaxMatch
MaxMatch starts at the first character of a sentence 
and tries to find the longest valid word starting from that character. 
If no word is found, the first character is deemed the longest "word", 
regardless of its validity. In order to find the rest of the words, 
MaxMatch is then recursively invoked on all of the remaining characters until no characters remain. 
A list of all of the words that were found is returned.

So for the string "happyday", "happy" is found because "happyday" is not a valid word, nor is "happyda", nor "happyd". Then, MaxMatch is called on "day", and "day" is found. The output is the list ["happy", "day"] in that order.

The Challenge
Write maxMatch, 
which takes an alphanumeric, spaceless, lowercased String as input and returns a [String] of all the words found, 
in the order they were found. 
All valid words are in the [String] validWords, which only contains around 500 English words.

Note: This algorithm is simple and operates better on Chinese text, 
so accept the fact that some words will be segmented wrongly.
-}
-----------------------------------------------------------------------------------------------------------


import qualified Data.Map.Lazy as Map
import Data.Maybe

import MaxMatch.Preloaded (validWords)

data Trie a = Trie Bool (Map.Map a (Trie a)) deriving (Eq, Show)

emptyTrie :: Trie a
emptyTrie = Trie False Map.empty

insertTrie :: Ord a => [a] -> Trie a -> Trie a
insertTrie []       (Trie _ nodes)   = Trie True nodes
insertTrie (x : xs) (Trie end nodes) = Trie end (Map.alter (Just . insertTrie xs . fromMaybe emptyTrie) x nodes)

buildTrie :: Ord a => [[a]] -> Trie a
buildTrie as = buildTrie' as emptyTrie where
    buildTrie' []       trie = trie
    buildTrie' (x : xs) trie = buildTrie' xs $ insertTrie x trie

lookupLongest :: Ord a => [a] -> Trie a -> ([a], [a])
lookupLongest = lookupLongest' [] [] where
    lookupLongest' ys rs []       (Trie end _)     = if end then (rs ++ reverse ys, []) else (rs, reverse ys)
    lookupLongest' ys rs (x : xs) (Trie end nodes) =
        fromMaybe (rs', reverse ys' ++ xs) (lookupLongest' ys' rs' xs <$> Map.lookup x nodes) where
            (ys', rs') = if end then ([x], rs ++ reverse ys) else (x : ys, rs)

dictionary = buildTrie validWords

maxMatch :: String -> [String]
maxMatch "" = []
maxMatch ss @ (x : xs) = if null ws then [x] : maxMatch xs else ws : maxMatch rs where
    (ws, rs) = lookupLongest ss dictionary
