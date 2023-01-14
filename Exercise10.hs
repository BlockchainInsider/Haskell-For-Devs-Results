module Exercise10 where


-- Exercise 10.1
-----------------------------------------------------------------------------------------------------------
{- 
Consider the number 1176 and its square (1176 * 1176) = 1382976. Notice that:

the first two digits of 1176 form a prime.
the first two digits of the square 1382976 also form a prime.
the last two digits of 1176 and 1382976 are the same.
Given two numbers representing a range (a, b), 
how many numbers satisfy this property within that range? (a <= n < b)

Example
solve(2, 1200) = 1, because only 1176 satisfies this property within the range 2 <= n < 1200. 
See test cases for more examples. The upper bound for the range will not exceed 1,000,000.

Good luck!
-}
-----------------------------------------------------------------------------------------------------------


isPrime :: Int -> Bool
isPrime 0 = False
isPrime 1 = False
isPrime x = if x < 0 then False else foldr (&&) True $ map (\y -> x `mod` y /= 0) [2 .. ceiling $ sqrt $ fromIntegral (x - 1)]

satisfy :: Int -> Bool
satisfy a = let sq = show (a * a)
                a' = show a
             in
        isPrime (read $ take 2 sq :: Int)
    &&  isPrime (read $ take 2 a' :: Int)
    &&  (drop (length sq - 2) sq) == (drop (length a' - 2) a')

solve :: Int -> Int -> Int
solve a b =  length $ filter (\x -> satisfy x) [a .. b]


-- Exercise 10.2
-----------------------------------------------------------------------------------------------------------
{- 
You wrote a program that can calculate the sum of all the digits of a non-negative integer.

However, it's not fast enough. Can you make it faster?
-}
-----------------------------------------------------------------------------------------------------------


digitSum :: Integer -> Integer
digitSum = fromIntegral . go 0
  where
    go a 0 = a
    go a n = go (a + go' 0 l') h
      where
        (h, l) = divMod n 100000000
        l' :: Int
        l' = fromIntegral l
    go' a 0 = a
    go' a n = go' (a + l) h
      where
        (h, l) = divMod n 10


-- Exercise 10.3
-----------------------------------------------------------------------------------------------------------
{-
Write a function that returns a list of all the integers from lower ( inclusive ) to upper ( exclusive ) 
in a such way that no two adjacent numbers in the list are numerically adjacent ( e.g. 5 cannot be next to 4 or 6 ).

The maximum sequence length tested is 107. The minimum sequence length tested is long enough.

Examples
For solution(0,6): 5,0,4,1,3,2 would not be acceptable, because 3 and 2 are adjacent. 0,2,5,3,1,4 would be acceptable.
-}
-----------------------------------------------------------------------------------------------------------


import Data.List

list :: Int -> Int -> [Int]
list a b = xs ++ ys
  where (xs,ys) = partition even [a..b-1]    


-- Exercise 10.4
-----------------------------------------------------------------------------------------------------------
{- 
If we write out the digits of "60" as English words we get "sixzero"; 
the number of letters in "sixzero" is seven. The number of letters in "seven" is five. 
The number of letters in "five" is four. The number of letters in "four" is four: 
we have reached a stable equilibrium.

Note: for integers larger than 9, 
write out the names of each digit in a single word (instead of the proper name of the number in English). 
For example, write 12 as "onetwo" (instead of twelve), and 999 as "nineninenine" 
(instead of nine hundred and ninety-nine).

For any integer between 0 and 999, return an array showing the path from that integer to a stable equilibrium:

Examples
numbersOfLetters 60 -> ["sixzero", "seven", "five", "four"]
numbersOfLetters 1 -> ["one", "three", "five", "four"]
-}
-----------------------------------------------------------------------------------------------------------


import Data.Char (digitToInt)

numbersOfLetters :: Int -> [String]
numbersOfLetters n
  | numberInWords == wordsLengthInWords = [numberInWords]
  | wordsLength == length wordsLengthInWords = [numberInWords, wordsLengthInWords]
  | otherwise = numberInWords : numbersOfLetters wordsLength
  where
    numberInWords = inWords n
    wordsLength = length numberInWords
    wordsLengthInWords = inWords wordsLength

inWords :: Int -> String
inWords = concatMap (numbers !!) . toDigits

numbers = ["zero", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]

toDigits :: Int -> [Int]
toDigits = map digitToInt . show


-- Exercise 10.5
-----------------------------------------------------------------------------------------------------------
{- 
Consider the following series:

1, 2, 4, 8, 16, 22, 26, 38, 62, 74, 102, 104, 108, 116, 122

It is generated as follows:

For single digit integers, add the number to itself to get the next element.
For other integers, multiply all the non-zero digits 
and add the result to the original number to get the next element.
For example: 16 + (6 * 1) = 22 and 104 + (4 * 1) = 108.

Let's begin the same series with a seed value of 3 instead of 1:

3, 6, 12, 14, 18, 26, 38, 62, 74, 102, 104, 108, 116, 122

Notice that the two sequences converge at 26 and are identical therefter. 
We will call the series seeded by a value of 1 the "base series" and the other series the "test series".

Let's look another test series that starts with 15

15, 20, 22, 26, 38, 62, 74, 102, 104, 108, 116, 122

The sequences converge at 22 if the test series starts with 15

You will be given a seed value for the test series 
and your task will be to return the number of integers 
that have to be generated in the test series before it converges to the base series. In the case above:

convergence(3) = 5, the length of [3, 6, 12, 14, 18]. 
convergence(15) = 2, the length of [15, 20]. 
-}
-----------------------------------------------------------------------------------------------------------


import Data.Char
import Data.List

convergence :: Int -> Int
convergence = (go (series 1)) . series
  where
    go xs (y:ys) =
      if (last $ takeWhile (<= y) xs) == y
        then 0
        else 1 + go xs ys

series = iterate go
  where
    go x =
      case map digitToInt $ show x of
        [a] -> a + a
        as -> x + (product $ filter (/= 0) as)


-- Exercise 10.6
-----------------------------------------------------------------------------------------------------------
{- 
Your goal is to create a function to format a number given a template; 
if the number is not present, use the digits 1234567890 to fill in the spaces.

A few rules:

the template might consist of other numbers, special characters or the like: 
you need to replace only alphabetical characters (both lower- and uppercase);
if the given or default string representing the number is shorter than the template, 
just repeat it to fill all the spaces.
A few examples:

numericFormatter "xxx xxxxx xx" "5465253289" == "546 52532 89"
numericFormatter "xxx xxxxx xx" "" == "123 45678 90"
numericFormatter "+555 aaaa bbbb" "18031978" == "+555 1803 1978"
numericFormatter "+555 aaaa bbbb" "" == "+555 1234 5678"
numericFormatter "xxxx yyyy zzzz" "" == "1234 5678 9012"
-}
-----------------------------------------------------------------------------------------------------------


import Data.Char (isLetter)

numericFormatter :: String -> String -> String
numericFormatter template [] = format template . tail . cycle $ ['0' .. '9']
numericFormatter template number = format template (cycle number)

format :: String -> String -> String
format [] _ = []
format (t : template) (n : number)
  | isLetter t = n : numericFormatter template number
  | otherwise = t : numericFormatter template (n : number)


-- Exercise 10.7
-----------------------------------------------------------------------------------------------------------
{-
The Pied Piper has been enlisted to play his magical tune and coax all the rats out of town.

But some of the rats are deaf and are going the wrong way!

Task
How many deaf rats are there?

Legend
P = The Pied Piper
O~ = Rat going left
~O = Rat going right
Example
ex1 ~O~O~O~O P has 0 deaf rats

ex2 P O~ O~ ~O O~ has 1 deaf rat

ex3 ~O~O~O~OP~O~OO~ has 2 deaf rats
-}
-----------------------------------------------------------------------------------------------------------


import Data.List

countDeafRats :: String -> Int
countDeafRats s = res "~O" cl 0
  where cl = filter (\x -> x == '~' || x == 'O' || x == 'P') s
        res t l i | null l = i
                  | head l == 'P' = res (reverse t) (tail l) i
                  | t `isPrefixOf` l = res t (drop 2 l) i
                  | otherwise = res t (drop 2 l) (i+1)


-- Exercise 10.8
-----------------------------------------------------------------------------------------------------------
{- 
Return the most profit from stock quotes.

Stock quotes are stored in an array in order of date. 
The stock profit is the difference in prices in buying and selling stock. 
Each day, you can either buy one unit of stock, sell any number of stock units you have already bought, or do nothing. 
Therefore, the most profit is the maximum difference of all pairs in a sequence of stock prices.

@param {array} quotes
@return {number} max profit
Example:

 [ 1, 2, 3, 4, 5, 6 ]        => 15  (buy at 1,2,3,4,5 and then sell all at 6)
 [ 6, 5, 4, 3, 2, 1 ]        => 0   (nothing to buy for profit)
 [ 1, 6, 5, 10, 8, 7 ]       => 18  (buy at 1,6,5 and sell all at 10)
 [ 1, 2, 10, 3, 2, 7, 3, 2 ] => 26  (buy at 1,2 and sell them at 10. Then buy at 3,2 and sell them at 7)
-}
-----------------------------------------------------------------------------------------------------------



import Data.List
import Data.Ord

profit :: [Int] -> Int
profit [] = 0
profit xs = m * length ls - sum ls + profit rs where
    (ls, m : rs) = splitAt (maxIndex xs) xs

maxIndex ::  Ord a => [a] -> Int
maxIndex = fst . maximumBy (comparing snd) . zip [0..]


-- Exercise 10.9
-----------------------------------------------------------------------------------------------------------
{- 
Due to lack of maintenance the minute-hand has fallen off Town Hall clock face.

And because the local council has lost most of our tax money to a Nigerian email scam 
there are no funds to fix the clock properly.

Instead, they are asking for volunteer programmers to write some code that tell the time 
by only looking at the remaining hour-hand!

What a bunch of cheapskates!

Can you do it?

Given the angle (in degrees) of the hour-hand, return the time in 12 hour HH:MM format. 
Round down to the nearest minute.

Examples
12:00 = 0 degrees

03:00 = 90 degrees

06:00 = 180 degrees

09:00 = 270 degrees

12:00 = 360 degrees

Notes
0 <= angle <= 360

Do not make any AM or PM assumptions for the HH:MM result. They are indistinguishable for this Kata.

3 o'clock is 03:00, not 15:00
7 minutes past midnight is 12:07
7 minutes past noon is also 12:07
-}
-----------------------------------------------------------------------------------------------------------


import Text.Printf (printf)

whatTimeIsIt :: Float -> String
whatTimeIsIt angle
  | fullHours == 0 = printf "12:%02d" fullMinutes
  | otherwise = printf "%02d:%02d" fullHours fullMinutes
  where
    fullHours = (`mod` 12) . floor . (/ 30.0) $ angle :: Int
    hoursAngle = fromIntegral fullHours * 30.0 :: Float
    fullMinutes = (`mod` 60) . floor . (* 60) . (/ 30) $ angle - hoursAngle :: Int


-- Exercise 10.10
-----------------------------------------------------------------------------------------------------------
{- 
Given two strings, the first being a random string and the second being the same as the first, 
but with three added characters somewhere in the string (three same characters),

Write a function that returns the added character

E.g
string1 = "hello"
string2 = "aaahello"

// => 'a'
The above is just an example; the characters could be anywhere in the string and string2 is actually shuffled.

Another example
string1 = "abcde"
string2 = "2db2a2ec"

// => '2'
Note that the added character could also exist in the original string

string1 = "aabbcc"
string2 = "aacccbbcc"

// => 'c'
You can assume that string2 will aways be larger than string1, 
and there will always be three added characters in string2.
-}
-----------------------------------------------------------------------------------------------------------


import Data.List (sort)

addedChar :: String -> String -> Char
addedChar xs ys = head $ sort ys `without` sort xs

without :: String -> String -> String
without xs [] = xs
without [] _ = []
without (x : xs) (y : ys)
  | y == x = without xs ys
  | otherwise = x : without xs (y : ys)


-- Exercise 10.11
-----------------------------------------------------------------------------------------------------------
{- 
Re-order the characters of a string, 
so that they are concatenated into a new string in "case-insensitively-alphabetical-order-of-appearance" order. 
Whitespace and punctuation shall simply be removed!

The input is restricted to contain no numerals and only words containing the english alphabet letters.

Example:

alphabetized "The Holy Bible" -- "BbeehHilloTy"
-}
-----------------------------------------------------------------------------------------------------------



import Data.Char (toLower, isLetter)
import Data.List (sortOn)

alphabetized :: String -> String
alphabetized = filter isLetter . sortOn toLower


-- Exercise 10.12
-----------------------------------------------------------------------------------------------------------
{- 
A list of integers is sorted in “Wave” order if 
alternate items are not less than their immediate neighbors 
(thus the other alternate items are not greater than their immediate neighbors).

Thus, the array [4, 1, 7, 5, 6, 2, 3] is in Wave order because 4 >= 1, 
then 1 <= 7, then 7 >= 5, then 5 <= 6, then 6 >= 2, and finally 2 <= 3.

The wave-sorted lists has to begin with an element not less than the next, 
so [1, 4, 5, 3] is not sorted in Wave because 1 < 4

Your task is to implement a function that takes a list of integers and sorts it into wave order in place; 
your function shouldn't return anything.

Note:

The resulting array shouldn't necessarily match anyone in the tests,
a function just checks if the array is now wave sorted.
-}
-----------------------------------------------------------------------------------------------------------


import Data.List (sort, transpose)
import Preloaded (isWaveSorted)

waveSort :: (Ord x) => [x] -> [x]
waveSort = toWave . sort

toWave :: (Ord x) => [x] -> [x]
toWave xs
  | isWaveSorted wave = wave
  | otherwise = concatMap reverse transposed
  where
    count = length xs
    (first, second) = splitAt (count `div` 2) xs
    transposed = transpose [first, second]
    wave = concat transposed


-- Exercise 10.13
-----------------------------------------------------------------------------------------------------------
{- 
Here are some children, each with a certain number of apples. 
Now we have to do something to make the number of apples of each child are equal.

We need to complete the redistribution step by step. 
With each step, we can transfer two apples from one child to another (whether or not adjacent does not matter).

Given an array apples that represents the number of apples per child. 
Your task is to calculate the minimum step of the transition operation. If it's impossible, return Nothing.

Example
For apples=[7,15,9,5], the output should be Just 3.

                       7 15 9 5
step 1: 15-2, 5+2 ---> 7 13 9 7
step 2: 13-2, 7+2 ---> 9 11 9 7
step 3: 11-2, 7+2 ---> 9  9 9 9
For apples=[7,7,7,7], the output should be Just 0.

The children's apples are equal already. No need more step.

For apples=[7,7,7,5], the output should be Nothing.

26 apples can't be divided equally between 4 children.
-}
-----------------------------------------------------------------------------------------------------------


minSteps :: [ Int ] -> Maybe Int
minSteps initialDistribution
  | numberOfApples `rem` numberOfChildren /= 0 = Nothing
  | any odd distributionDifferences = Nothing
  | otherwise = Just numberOfRedistributionSteps
  where

  numberOfApples = sum initialDistribution :: Int
  numberOfChildren = length initialDistribution :: Int

  fairDistribution = replicate <$> id <*> div numberOfApples $ numberOfChildren :: [ Int ]

  distributionDifferences = zipWith subtract initialDistribution fairDistribution :: [ Int ]

  numberOfRedistributionSteps = ( `div` 2 ) . sum . map ( flip div 2 . abs ) $ distributionDifferences :: Int


-- Exercise 10.14
-----------------------------------------------------------------------------------------------------------
{- 
You will be given two numbers m,n. The numbers could span from 0 to 10000. 
We can get their product by using binary reduction as show in the table below.

Example (to understand the table please read the description below it)

real value of m(r)  m   n   (r*n)
0                   100 15    0
0                   50  30    0
1                   25  60    60
0                   12  120   0
0                   6   240   0
1                   3   480   480
1                   1   960   960
Above, we are given two numbers 100 and 15. 
we keep reducing the bigger number by dividing it by 2 and hold the integer part of the division 
till it is no more divisible by 2. Then we assign the real values to these reduced parts of m. 
Any reduced number [1,m] has real value of 0 if it is even, and it will have real value of 1 if it is odd. 
On the other hand the smaller number in this case n keeps on doubling itself the same amount of times m reduces itself. 
The idea of this method is to change multiplication of two big number to 
a sequence of multiplication by 0 or 1 and perform addition to get the final product. 
You can see that from the last column (r*n) above.
if we sum the last column we get 0+60+0+0+480+960=1500=100*15 
Now your task will be to get those non-zero number in the last column in an array 
and return it sorted in descending order.so for the above example the return will be [960,480,60].

Beware: m,n are not necessarily ordered.
-}
-----------------------------------------------------------------------------------------------------------


binMul :: Int -> Int -> [Int]
binMul 0 _ = []
binMul _ 0 = []
binMul m n = if m>n then binMul' m n [] else binMul' n m []

binMul' :: Int -> Int -> [Int] -> [Int]
binMul' 1 n xs = n:xs
binMul' m n xs = if m `mod` 2 == 0
                 then binMul' (m `div` 2) (n*2) xs
                 else binMul' (m `div` 2) (n*2) (n:xs)


-- Exercise 10.15
-----------------------------------------------------------------------------------------------------------
{- 
Given an D-dimension array, where each axis is of length N, 
your goal is to find the sum of every index in the array starting from 0.

For Example if D=1 and N=10 then the answer would be 45 ([0,1,2,3,4,5,6,7,8,9]) 
If D=2 and N = 3 the answer is 18 which would be the sum of every number in the following:

[
[(0,0), (0,1), (0,2)],
[(1,0), (1,1), (1,2)],
[(2,0), (2,1), (2,2)]
]
A naive solution could be to loop over every index in every dimension and add to a global sum. 
This won't work as the number of dimension is expected to be quite large.

Hint: A formulaic approach would be best 
Hint 2: Gauss could solve the one dimensional case in his earliest of years, 
This is just a generalization.
-}
-----------------------------------------------------------------------------------------------------------


superSum :: Integer -> Integer -> Integer
superSum 1 n = (n - 1) * n `div` 2
superSum d n = (superSum (d-1) n) * n + (superSum 1 n) * (n ^ (d - 1))


-- Exercise 10.16
-----------------------------------------------------------------------------------------------------------
{- 
You are given a string of numbers between 0-9. 
Find the average of these numbers and return it as a floored whole number (ie: no decimal places) 
written out as a string. Eg:

"zero nine five two" -> "four"

If the string is empty or includes a number greater than 9, return "n/a"
-}
-----------------------------------------------------------------------------------------------------------


import Data.Map ( Map, fromList, member, (!) )

cardinals :: [String]
cardinals = ["zero", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine"] 

dict :: Map String Int
dict = fromList $ zip cardinals [0..]

averageString :: String -> String
averageString s
    | null s = "n/a"
    | all (`member` dict) s' = cardinals !! (sum (map (dict !) s') `div` length s')
    | otherwise = "n/a"
  where
    s' = words s


-- Exercise 10.17
-----------------------------------------------------------------------------------------------------------
{- 
Create a function eqAll that determines if all elements of a list are equal.
list can be a list of any Eq instance but you need not support any other Foldables; 
return value is a Bool.

Examples
eqAll "aaaaa" -> True
eqAll "abcde" -> False
eqAll [0,0,0] -> True
eqAll [0,1,2] -> False

eqAll "" -> True
eqAll [] -> True
-}
-----------------------------------------------------------------------------------------------------------


eqAll :: (Eq a) => [a] -> Bool
eqAll [] = True
eqAll (x : xs) = all (== x) xs


-- Exercise 10.18
-----------------------------------------------------------------------------------------------------------
{- 
International hackers group organized a programming competition, in which n teams participated.

They were assigned to separate rooms for competitions, and the rooms were lined up in a straight line.

The game was over and each team scored points. It's time to pay bonuses. The rule is:

- The bonus unit is 1K($1000), and each team gets at least 1k.
- The bonus payment process is not public. 
- A team can know the bonus amount of its adjacent team, if the 
  score of the adjacent team is lower than itself.
- If a team finds that its bonus is no higher than the adjacent team whose
  score is lower than itself, the team will not be satisfied
Given an integer array scores represents the score of all teams. 
Your task is to calculate how much bonuses international hackers group need to pay to keep all teams satisfied.

Note, the unit of bonus is 1K. All teams are in a straight line, 
and their order is the same as that of the array elements.

Example
For scores = [10,20,30], the output should be 6.

team1's score = 10
team2's score = 20
team3's score = 30

team1 can get 1K, The team was satisfied 
because it knew nothing about the other teams.

team2 can know team1's bonus amount, 
So team2 at least get 2K to be satisfied

team3 can know team2's bonus amount, 
So team3 at least get 3K to be satisfied

1 + 2 + 3 = 6
For scores = [10,20,20,30], the output should be 6.

The possible bonus amount of each team can be:[1,2,1,2]

For scores = [20,30,10,30,40,10,20,30,40,30], the output should be 20.

The possible bonus amount of each team can be:[1,2,1,2,3,1,2,3,4,1]
-}
-----------------------------------------------------------------------------------------------------------


minimumBonus :: [Int] -> Int
minimumBonus [] = 0
minimumBonus [x] = 1
minimumBonus (x:xs) = sum (migliora [] (uni (x:xs)) [] (structure x xs))



structure _ [] = []
structure l (r:rs) | l < r = '<' : structure r rs
 | l == r = '=' : structure r rs
 | otherwise = '>' : structure r rs


uni :: [a] -> [Int]
uni = foldr (\ _  -> (1:) ) []


migliora seen [] _ _ = reverse seen
migliora [] (x:xs) sint str = migliora [x] xs sint str
migliora (s:seen) (x:xs) sint ('>':str) | s > x = migliora (x:(s:seen)) xs ('>':sint) str
 | otherwise = migliora [] ((reverse seen) ++ (succ x :(x:xs))) [] ((reverse sint) ++ ('>':str))
migliora (s:seen) (x:xs) sint ('<':str) | s < x = migliora (x:(s:seen)) xs ('<':sint) str
 | otherwise = migliora [] ((reverse seen) ++ (s :(succ s:xs))) [] ((reverse sint) ++ ('<':str))
migliora (s:seen) (x:xs) sint ('=':str) = migliora (x:(s:seen)) xs ('=':sint) str


-- Exercise 10.19
-----------------------------------------------------------------------------------------------------------
{- 
You will receive an array as parameter that contains 1 or more integers and a number n.

Here is a little visualization of the process:

Step 1: Split the array in two:

[1, 2, 5, 7, 2, 3, 5, 7, 8]
      /            \
[1, 2, 5, 7]    [2, 3, 5, 7, 8]
Step 2: Put the arrays on top of each other:

   [1, 2, 5, 7]
[2, 3, 5, 7, 8]
Step 3: Add them together:

[2, 4, 7, 12, 15]
Repeat the above steps n times or until there is only one number left, and then return the array.

Example
Input: arr=[4, 2, 5, 3, 2, 5, 7], n=2


Round 1
-------
step 1: [4, 2, 5]  [3, 2, 5, 7]

step 2:    [4, 2, 5]
        [3, 2, 5, 7]

step 3: [3, 6, 7, 12]


Round 2
-------
step 1: [3, 6]  [7, 12]

step 2:  [3,  6]
         [7, 12]

step 3: [10, 18]


Result: [10, 18]
-}
-----------------------------------------------------------------------------------------------------------


import Data.Maybe

splitAndAdd :: [Int] -> Int -> [Int]
splitAndAdd ns 0 = ns
splitAndAdd (n:[]) _ = [n]
splitAndAdd ns max = splitAndAdd nextNs (max - 1)
  where
    half = length ns `div` 2
    as = take half ns
    b = if length ns `mod` 2 == 1
        then Just [head $ drop half ns]
        else Nothing
    cs = drop (half + if isJust b then 1 else 0) ns
    nextNs = (fromMaybe [] b) ++ zipWith (+) as cs


-- Exercise 10.20
-----------------------------------------------------------------------------------------------------------
{- 
you should guess an unknown number array.
You'll be given a function f, which can answer your query in this way:

f a b -- returns an Integral
a and b are indexes of two different elements, and f will return the sum of these two elements.

When a equals b or abs (a - b) is larger than 2, f will raise an error.

Your goal is to find out the correct array.

The whole procedure is like:

Get the f and the length of the array
Ask f questions
Give the correct array according to the answers
The array will always be 3 or longer than 3.
-}
-----------------------------------------------------------------------------------------------------------


guess :: Integral n => (Int -> Int -> n) -> Int -> [n]
guess f n = let threeSum = div (f 0 1 + f 1 2 + f 0 2) 2 in
  let zero = threeSum - f 1 2
      one  = threeSum - f 0 2
      two  = threeSum - f 0 1 in
    zero : one : findN (n - 2) two
  where findN 0  _ = []
        findN i' l = let i = n - i' in
          l : (findN (i' - 1) $ (f i $ i + 1) - l)
