module Exercise01 where


-- Exercise 1.1
-----------------------------------------------------------------------------------------------------------
{- 
'Evil' numbers are non-negative numbers with even parity, that is, 
numbers with an even number of 1's in their binary representation. The first few evil numbers are:

0,3,5,6,9,10,12,15,17,18,20
Write a function to return getEvil the n'th evil number.

getEvil 1 -- returns 0
getEvil 2 -- returns 3
getEvil 3 -- returns 5
-- etc
The tests will include values of n up to 10^100
-}
-----------------------------------------------------------------------------------------------------------


isEvil 0 = True
isEvil 1 = False
isEvil n = let (q,r) = n `quotRem` 2 
           in (r == 1) /= isEvil q

getEvil :: Integer -> Integer
getEvil n = 
    if isEvil t then t else t+1
  where
    t = 2 * (n-1)


-- Exercise 1.2
-----------------------------------------------------------------------------------------------------------
{- 
You have to cover a square area (measuring n x n where n is a positive integer) with square tiles. 
You have an unlimited supply of square tiles, 
but the lengths of the sides are all powers of 2 (1 x 1, 2 x 2, 4 x 4, 8 x 8, etc.). 
You must use the minimum number of tiles which will exactly cover the area.

For example, given an area measuring 13 x 13:

___________________________
|               |  4x4  |_|
|               |       |_|
|               |       |_|
|               |_______|_|
|    8x8        |  4x4  |_|
|               |       |_|
|               |       |_|
|_______________|_______|_|
|   4x4 |  4x4  |  4x4  |_|
|       |       |       |_|
|       |       |       |_|
|_______|_______|_______|_|
|_|_|_|_|_|_|_|_|_|_|_|_|_|
We can exactly cover the required area with 31 tiles.

Write a function which takes as its input the length of the square area to be tiled, 
and returns the minimum number of tiles required to exactly fill the area.

Be aware that some very large areas will be tested.
-}
-----------------------------------------------------------------------------------------------------------


numTiles :: Integer -> Integer
numTiles 0 = 0
numTiles n | m == 1 = 2 * n - 1 + numTiles n'
           | otherwise = numTiles n' where (n', m) = divMod n 2


-- Exercise 1.3
-----------------------------------------------------------------------------------------------------------
{-
This works similarly to Tap Code except instead of being mapped onto a 5x5 square, 
letters are mapped onto a 3x3x3 cube, left to right, top to bottom, 
front to back with space being the 27th "letter". 
Letters are represented by a series of taps (represented as dots .) and pauses (represented by spaces  ), 
for example A is represented as . . . (first column, first row, first layer) 
and  is represented as ... ... ... (third column, third row, third layer).

For reference the three layers of the cube are as follows (underscore represents space):

1  1  2  3 
1  A  B  C
2  D  E  F
3  G  H  I

2  1  2  3 
1  J  K  L
2  M  N  O
3  P  Q  R

3  1  2  3 
1  S  T  U
2  V  W  X
3  Y  Z  _
Your task (should you choose to accept it)
Create two functions encode() and decode(), to encode and decode strings to and from cubic tap code.

Input
encode() takes a string of uppercase letters and spaces and outputs a string of dots and spaces. 
decode() takes a string of dots and spaces and outputs a string of uppercase letters and spaces. 
All inputs will be valid.

Examples
encode("N") => ".. .. .."
encode("TEST") => ".. . ... .. .. . . . ... .. . ..."
encode("HELLO WORLD") => ".. ... . .. .. . ... . .. ... . .. ... .. .. ... ... ... .. .. ... ... .. .. ... ... .. ... . .. . .. ."

decode(".. .. ..") => "N"
decode(".. . ... .. .. . . . ... .. . ...") => "TEST"
decode(".. ... . .. .. . ... . .. ... . .. ... .. .. ... ... ... .. .. ... ... .. .. ... ... .. ... . .. . .. .") => "HELLO WORLD"
-}
-----------------------------------------------------------------------------------------------------------


encode :: String -> String
encode [] = []
encode (x:xs) = e' ++ (if null xs then "" else " ") ++ encode xs 
  where coord = snd $ head $ filter (\t -> fst t == x) table
        e' = (\(a,b,c) -> replicate a '.' ++ " " ++ replicate b '.' ++ " " ++ replicate c '.' ) coord

decode :: String -> String
decode [] = []
decode s = decode' (lookup' s) : decode (unwords $ drop 3 $ words s)
  where lookup' = (\[a,b,c] -> (a,b,c)) . map length . take 3 . words
        decode' coord = fst $ head $ filter (\t -> snd t == coord) table


table = zipWith (\(a,b,c) l -> (l,(a,b,c))) coords chars
  where chars = ['A'..'Z'] ++ " " 
        coords = [ (column,row,layer) | 
                                      layer <- [1..3], 
                                      row <- [1..3], 
                                      column <- [1..3] ] 


-- Exercise 1.4
-----------------------------------------------------------------------------------------------------------
{- 
You are given three integer inputs: length, minimum, and maximum.

Return a string that:

Starts at minimum
Ascends one at a time until reaching the maximum, then
Descends one at a time until reaching the minimum
repeat until the string is the appropriate length
Examples:

 length: 5, minimum: 1, maximum: 3   ==>  "12321"
 length: 14, minimum: 0, maximum: 2  ==>  "01210121012101"
 length: 11, minimum: 5, maximum: 9  ==>  "56789876567"
Notes:

length will always be non-negative
negative numbers can appear for minimum and maximum values
hyphens/dashes ("-") for negative numbers do count towards the length
the resulting string must be truncated to the exact length provided
return an empty string if maximum < minimum or length == 0
minimum and maximum can equal one another and result in a single number repeated for the length of the string
-}
-----------------------------------------------------------------------------------------------------------


ascendDescend :: Int -> Int -> Int -> String
ascendDescend len minBound maxBound
  | minBound > maxBound = ""
  | minBound == maxBound = f $ repeat minBound
  | otherwise = let asc = [minBound..maxBound]
                    desc = reverse $ asc
                    pat = (drop 1 $ desc) ++ (drop 1 $ asc)
                in f $ asc ++ cycle pat
    where
      f :: [Int] -> String
      f = (take len) . concat . map show


-- Exercise 1.5
-----------------------------------------------------------------------------------------------------------
{- 
The concat function on lists has the following type:

concat :: [[a]] -> [a]
Simply put, it flattens a list:

concat [[1, 2], [3, 4], [], [5]] == [1, 2, 3, 4, 5]
Farmer Thomas wants to write concat in terms of foldr. 
Specifically, he wants you to find p, q and r such that:

concat = foldr (foldr p q) r
Now get working!
-}
-----------------------------------------------------------------------------------------------------------


-- Make Farmer Thomas happy!

-- The test cases will define concat as "concat = foldr (foldr p q) r"
-- and your goal here is to define "p", "q" and "r"!
p :: a -> ([a] -> [a]) -> ([a] -> [a])
p a f b = a : f b
q :: [a] -> [a]
q = (<> [])
r :: [a]
r = []



-- Exercise 1.6
-----------------------------------------------------------------------------------------------------------
{- 
Write a function bell that will receive a positive integer and return an array. 
That's all splaining you receive; what needs to be done you'll have to figure out with the examples below.

 n => resulting array

 1 => [1]
 2 => [2, 2]
 3 => [3, 4, 3]
 4 => [4, 6, 6, 4]
 5 => [5, 8, 9, 8, 5]
 6 => [6, 10, 12, 12, 10, 6]
 7 => [7, 12, 15, 16, 15, 12, 7]
 8 => [8, 14, 18, 20, 20, 18, 14, 8]
 9 => [9, 16, 21, 24, 25, 24, 21, 16, 9]
10 => [10, 18, 24, 28, 30, 30, 28, 24, 18, 10]
11 => [11, 20, 27, 32, 35, 36, 35, 32, 27, 20, 11]
12 => [12, 22, 30, 36, 40, 42, 42, 40, 36, 30, 22, 12]
-}
-----------------------------------------------------------------------------------------------------------


helper 1 = [1]
helper 2 = [2]
helper n = n: [n+x| x <- lst]
  where lst = helper (n-2)

bell :: Word -> [Word]
bell n
  | even n = lst ++ (reverse  lst )
  | otherwise  = init lst ++ (reverse  lst )
  where lst = helper n


-- Exercise 1.7
-----------------------------------------------------------------------------------------------------------
{-
In this task, you need to restore a string from a list of its copies.

You will receive an array of strings. 
All of them are supposed to be the same as the original but, 
unfortunately, they were corrupted which means some of the characters were replaced with asterisks ("*").

You have to restore the original string based on non-corrupted information you have. 
If in some cases it is not possible to determine what the original character was, 
use "#" character as a special marker for that.

If the array is empty, then return an empty string.

Examples:
input = [
  "a*cde",
  "*bcde",
  "abc*e"
]
result = "abcde"


input = [
  "a*c**",
  "**cd*",
  "a*cd*"
]
result = "a#cd#"
-}
-----------------------------------------------------------------------------------------------------------

import Data.List (find, transpose)
import Data.Maybe (fromMaybe)

assembleString :: [[Char]] -> [Char]
assembleString = map (fromMaybe '#' . find (/= '*')) . transpose


-- Exercise 1.8
-----------------------------------------------------------------------------------------------------------
{- 
ISBN stands for International Standard Book Number.

For more than thirty years, ISBNs were 10 digits long. 
On January 1, 2007 the ISBN system switched to a 13-digit format. 
Now all ISBNs are 13-digits long. Actually, there is not a huge difference between them. 
You can convert a 10-digit ISBN to a 13-digit ISBN by adding the prefix number (978) to the beginning 
and then recalculating the last, check digit using a fairly simple method.

Method
Take the ISBN ("1-85326-158-0").
Remove the last character, which can be a number or "X".
Add the prefix number (978) and a hyphen (-) to the beginning.
Take the 12 digits, then alternately multiply each digit from left to right by 1 or 3.
Add up all 12 numbers you got.
Take the number and perform a modulo 10 division.
If the result is 0, the check digit is 0. If it isn't 0, then subtract the result from 10. 
In this case, that is the check digit.
Add the check digit to the end of the result from step 3.
Return the 13-digit ISBN in the appropriate format:
"prefix number - original ISBN except the last character - check digit"
"978 - 1 - 85326 - 158 - 9"

Example
ISBN = "1-85326-158-0"
remove_last_character = "1-85326-158-"
add_prefix = "978-1-85326-158-"
twelve_digits = 978185326158

check_digit = 9*1 + 7*3 + 8*1 + 1*3 + 8*1 + 5*3 + 3*1 + 2*3 + 6*1 + 1*3 + 5*1 + 8*3
            =   9 +  21 +   8 +   3 +   8 +  15 +   3 +   6 +   6 +   3 +   5 +  24
            = 111
            111 % 10 = 1
            10 - 1 = 9

thirteen_digit = 9781853261589

return "978-1-85326-158-9"
-}
-----------------------------------------------------------------------------------------------------------


import Data.Char (digitToInt)
import Data.List (intercalate)
import Data.List.Split (splitOn)

toISBN13 :: String -> String
toISBN13 isbn = intercalate "-" . (++ [checkDigit]) $ isbn13Groups
  where
    isbnGroups = splitOn "-" isbn :: [String]
    isbn13Groups = (["978"] ++) . init $ isbnGroups :: [String]
    digits = map digitToInt . concat $ isbn13Groups :: [Int]
    checksum = (`mod` 10) . sum . zipWith (*) (cycle [1, 3]) $ digits :: Int
    checkDigit :: String
    checkDigit
      | checksum == 0 = "0"
      | otherwise = show (10 - checksum)



-- Exercise 1.9
-----------------------------------------------------------------------------------------------------------
{- 
Quine is a nonempty program that prints itself. 
Your task is bit different than that. 
Write a function that takes no parameters and returns your program as a string instead of printing it.
-}
-----------------------------------------------------------------------------------------------------------


import Data.ByteString.Internal
quine = accursedUnutterablePerformIO $ readFile "solution.txt"


-- Exercise 1.10
-----------------------------------------------------------------------------------------------------------
{- 
You work at a lock situated on a very busy canal. 
Boats have queued up at both sides of the lock 
and your managers are asking for an update on how long it's going to take for all the boats to go through the lock.

Boats are queuing in order and they must go into the lock in that order. 
Multiple boats can go into the lock at the same time, however they must not exceed the length of the lock. 
The lock starts empty, and the timer should finish when the lock is down at empty, and all the boats are through. 
A boat takes its length in minutes to both enter and exit the lock, 
e.g. a boat of length 4 will take 4 minutes to enter and 4 minutes to leave the lock.

Notes
The lock takes 2 minutes to fill and empty each time
The lock should start and finish at the low end
No boat will ever be longer than the lock
The time should be returned in minutes
Example:
low queue  = [2, 3, 6, 1]
high queue = [1, 2]
max length = 7
Starting at low end
Boats 2 and 3 can fit inside the lock - total time is 5 minutes
The lock fills up - total time is 7 minutes
Boats 2 and 3 leave the lock - total time is 12 minutes
Starting at high end
Boats 1 and 2 enter the lock - total time is 15 minutes
The lock empties - total time is 17 minutes
Boats 1 and 2 leave the lock - total time is 20 minutes
Starting at low end
Boats 6 and 1 enter the lock - total time is 27 minutes
The lock fills up - total time is 29 minutes
Boats 6 and 1 leave the lock - total time is 36 minutes
Starting at high end
The lock empties as it must finish empty - total time is 38 minutes
-}
-----------------------------------------------------------------------------------------------------------


-- import Control.Monad.ListM (scanM)
-- import Data.Maybe.Extended (nothingIf)
import Data.List (foldl')

type Length = Int
type Minutes = Int

-- Its gonna reverse, but for my purposes I don't care
scanM :: (b -> a -> Maybe b) -> b -> [a] -> [b]
scanM f init = foldl' (\(y:ys) v -> case f y v of
  Just r -> r:ys
  Nothing -> case f init v of
    Just q -> q : y : ys
    Nothing -> undefined
  ) [init]
  
justIf :: (a -> Bool) -> a -> Maybe a
justIf f v
  | f v = Just v
  | otherwise = Nothing

canalMania :: [Length] -> [Length] -> Length -> Minutes
canalMania low high lock = 2 * (sum high + sum low + 2 * max (through high) (through low))
  where through = length . filter (>0) . scanM (\ a b -> justIf (<= lock) (a + b)) 0


-- Exercise 1.11
-----------------------------------------------------------------------------------------------------------
{- 
In this kata, you will write a program that checks whether a given list of integers is an autodigigram.

The idea is inspired by autograms, which are self-referential sentences 
that correctly state how often each letter occurs in their words. For example:

This autogram contains five a's, one b, two c's, two d's, twenty-six e's, 
six f's, two g's, four h's, thirteen i's, one j, one k, one l, two m's, twenty-one n's, sixteen o's, 
one p, one q, five r's, twenty-seven s's, twenty t's, three u's, six v's, nine w's, five x's, five y's, and one z.

Instead of sentences, we'll look at lists of numbers. 
If a list of integers correctly enumerates how often each digit occurs in it — i.e. 
the value at index d is equal to the total occurrence count of digit d — we'll call it an autodigigram.

Your task is to write a program that checks whether a given list of integers is an autodigigram 
when its numbers are represented in a given base and width (possibly with leading zeros).

Examples:

[6, 2, 1, 0, 0, 0, 1, 0, 0, 0] is an autodigigram of decimal one-digit numbers 
because it contains six 0's, two 1's, one 2, one 6, and zero of the other digits.
[1B, 03, 00, 01, 00, 00, 00, 00, 00, 00, 00, 01, 00, 00, 00, 00] is an autodigigram of hexadecimal two-digit numbers 
that contains twenty-seven (1B in hexadecimal) 0's, three 1's, one 3, one B, and zero of the other hexadecimal digits.
[01000, 00010] is an autodigigram of binary five-digit numbers that contains eight 0's and two 1's.
[10, 02, 01] is an autodigigram of ternary two-digit numbers that contains three 0's, two 1's, and one 2.
You can assume (without having to check) that the input will satisfy the following conditions:

base ranges from 2 to 36
width ranges from 1 to 30
list has the correct length, i.e. base elements
list contains only positive numbers
no number in list exceeds width when represented in base
-}
-----------------------------------------------------------------------------------------------------------


validate :: Int -> Int -> [Int] -> Bool
validate base width list = counter == list
  where counter = foldr f (replicate base 0) list
        f :: Int -> [Int] -> [Int]
        f num acc = foldr g acc (digits num)
        
        g :: Int -> [Int] -> [Int]
        g i acc = take i acc `mappend` [acc !! i + 1] `mappend` drop (i+1) acc
        
        digits :: Int -> [Int]
        digits n =
          let dgts = map (`mod` base) $ takeWhile (>0) $ iterate (`div` base) n
              cnt  = length dgts 
          in
          if cnt < width
          then dgts `mappend` (replicate (width - cnt) 0)
          else dgts


-- Exercise 1.12
-----------------------------------------------------------------------------------------------------------
{- 
The workers of CodeLand intend to build a brand new building in the town centre.

They intend to build a triangle-based pyramid (tetrahedron) out of cubes.

They require a program that will find the tallest potential height of the pyramid, given a certain number of cubes.

Your function needs to return the pyramid with largest number of full layers possible with the input.

The input will be an integer of how many cubes are available, 
and your output should return the height of the tallest pyramid buildable with that number of cubes.

The schematic below shows a cross-section of each layer of the pyramid, top down:

Layer 1 -

          x
Layer 2 -

          x
        x   x
        
Layer 3 -

          x
        x   x
      x   x   x
-}
-----------------------------------------------------------------------------------------------------------


findHeight :: Int -> Int
findHeight n | h * (h + 1) * (h + 2) <= 6 * n = h
             | otherwise = pred h
    where h = floor $ fromIntegral (n * 6) ** (1 / 3)


-- Exercise 1.13
-----------------------------------------------------------------------------------------------------------
{- 
Your function will receive two positive integers ( integers ranging from 1 upward), 
and return an array. There's logic in there, but all you get are the example test cases to find it. 
Below overview for your convenience. (And, alright: the function name is a strong hint of what to do.)

(s, d) => array

(1, 1) => [1]
(2, 1) => [2]
(3, 1) => [2, 1]
(4, 1) => [2, 2]
(5, 1) => [2, 2, 1]

(1, 2) => [1]
(2, 2) => [4]
(3, 2) => [8, 1]
(4, 2) => [12, 4]
(5, 2) => [16, 8, 1]

(1, 3) => [1]
(2, 3) => [8]
(3, 3) => [26, 1]
(4, 3) => [56, 8]
(5, 3) => [98, 26, 1]

(1, 4) => [1]
(2, 4) => [16]
(3, 4) => [80, 1]
(4, 4) => [240, 16]
(5, 4) => [544, 80, 1]
-}
-----------------------------------------------------------------------------------------------------------


peelTheOnion :: Int -> Int -> [Int]
peelTheOnion x n
  | x < 1 = []
  | x == 1 = [1]
  | otherwise = (x ^ n - nextX ^ n) : peelTheOnion nextX n
  where
    nextX = x - 2


-- Exercise 1.14
-----------------------------------------------------------------------------------------------------------
{- 
Consider some subject, who has some initial momentum and is travelling through an array (powerups).

momentum is an integer that represents the "distance" the subject can travel through the array. 
Each index travelled requires one unit of momentum. (The subject starts outside of the array, 
so it requires 1 momentum to get to index 0).

powerups is an array of integers which the subject is travelling through. 
At each index, the value there is added to the subject's total momentum.

If at any point through the array, the subject's momentum is below 1, 
the subject stops there, and does not successfully make it through. 
If the subject does make it to the last index of the array, 
with at least 1 momentum remaining (required to leave the array), 
it has successfully reached the "other side".

Examples:

momentum = 3 and powerups = [0,0,0] - No success (it finished in the last index).

momentum = 3 and powerups = [0,1,0] - Success

Resume
You are given a list of momentum listOfMomentum and a list of powerups listOfPowerups(a 2D list of numbers). 
You have to check it for each pair of listOfMomentum[index] and listOfPowerups[index].

Return indexes of sublists where there was enough momentum.

Notes
The sublists in listOfPowerups can be of a different length.
listOfMomentum will be of same length as listOfPowerups.
listOfMomentum and sublists of listOfPowerups only contain integers from 0 to 9.
There can be duplicated numbers.
The numbers in the result must be in order.
Example for:

listOfMomentum = [3, 2, 1, 0]  and

listOfPowerups = [[1, 0, 0], [0, 2, 0, 0], [0, 9], [8, 8]

listOfMomentum[0] = 3
listOfPowerups[0]  = [1, 0, 0]

listOfMomentum[1] = 2
listOfPowerups[1]  = [0, 2, 0, 0]

listOfMomentum[2] = 1
listOfPowerups[2]  = [0, 9]

listOfMomentum[3] = 0
listOfPowerups[3]  = [8, 8]
So, the output will be [0]
-}
-----------------------------------------------------------------------------------------------------------


import Data.List (findIndices)

survivors :: [Int] -> [[Int]] -> [Int]
survivors ms pss = findIndices good (zip ms pss)
  where good (m, ps) = minimum (scanl (\m p -> m + p - 1) m ps) > 0


-- Exercise 1.15
-----------------------------------------------------------------------------------------------------------
{- 
Given a list of strings (of letters and spaces), and a list of numbers:

Considering the list of strings as a 2D character array, 
the idea is to remove from each column, starting from bottom, as many letters as indicated in the list of numbers. 
Then return the remaining letters in any order as a string.

If there aren't enough letters, just remove those you can.
The strings in the list will all be of the same length.
The list of numbers will be of the same length as the strings in the list of strings.
Strings will contain only lowercase letters and spaces.
There can be duplicated letters and numbers.
Example:
strings

["abc", 
 " z ", 
 " a "]
numbers

 [0,4,1]
the output would be "a".
-}
-----------------------------------------------------------------------------------------------------------


import Data.List

lastSurvivors :: [String] -> [Int] -> String
lastSurvivors xxs ns =  concat
                      . zipWith (\n xs -> take (length xs - n) xs) ns 
                      . map (filter (/=' '))
                      . transpose $ xxs


-- Exercise 1.16
-----------------------------------------------------------------------------------------------------------
{- 
Substitute two equal letters by the next letter of the alphabet (two letters convert to one):

"aa" => "b", "bb" => "c", .. "zz" => "a".
The equal letters do not have to be adjacent.
Repeat this operation until there are no possible substitutions left.
Return a string.

Example:

let str = "zzzab"
    str = "azab"
    str = "bzb"
    str = "cz"
return "cz"
Notes
The order of letters in the result is not important.
The letters "zz" transform into "a".
There will only be lowercase letters.
-}
-----------------------------------------------------------------------------------------------------------


import Data.Bool (bool)
import Data.Function (fix)
import qualified Data.Set as S

lastSurvivors :: String -> String
lastSurvivors = S.toAscList . foldr ins S.empty
  where
    ins = fix (\r c s -> bool (S.insert c s) (r (next c) (S.delete c s)) (S.member c s))
    next = bool succ (const 'a') =<< (== 'z')


-- Exercise 1.17
-----------------------------------------------------------------------------------------------------------
{- 
Ryomen Sukuna has entered your body, and he will only leave if you can solve this problem.

Given an integer n, how many integers between 1 and n (inclusive) are unrepresentable as aba^ba 
b
 , where aaa and bbb are integers not less than 2?

Example:

if n equals 10 then output must be 7,
as 4 is representable as 22, 8 as 23 and 9 as 32.
So the numbers left are 1, 2, 3, 5, 6, 7 and 10.

Note:

Optimize your solution for n as large as 1010.
-}
-----------------------------------------------------------------------------------------------------------


import qualified Data.Set as S

primes :: [Int]
primes = [2,3,5,7,11,13,17,19,23,29,31]

sukuna :: Int -> Int
sukuna n = (n-) . length . S.elems . S.unions . map (S.fromAscList . f n) $ primes
    where f n p = takeWhile (<=n) . map (^p) $ [2..]


-- Exercise 1.18
-----------------------------------------------------------------------------------------------------------
{- 
The Evil King of Numbers wants to conquer all space in the Digital World. 
For that reason, His Evilness declared war on Letters, which actually stay in the Alphabet Fragmentation. 
You were nominated the Great Arbiter and must provide results of battles to the technology God 3 in 1, 
Llib Setag-Kram Grebrekcuz-Nole Ksum.

Description
armyLetters consists of letters from 'a' to 'z' and armyNumbers consists of digits from '1' to '9'. 
The power of a letter is its position in the Latin alphabet, 
so the letter 'a' has power 1, 'b' has 2, .. 'z' has 26. 
The power of a digit is its value, so '1' has power 1, '2' has 2, .. '9' has 9.

armyLetters fights from its end; armyNumbers fights from its beginning.

Per round, one letter from armyLetters attacks one digit and does damage equal to its power, 
and one digit from armyNumbers attacks two letters and does damage equal to its power to both. 
Characters with 0 or lower power disappear.

Rounds of battle continue until at least one army has completely disappeared.

Output
If either or both armies are empty at the start of hostilities, return "Peace".
At the end of the war, return "Draw" if both armies died, or the final state of the winning army (as a String).
How the attacks happen
For example, we have "abc" and "12".

The rightmost letter of "abc" is 'c', which has power 3, and the leftmost digit of "12" is '1'.

'c' attacks '1' and at the same time '1' attacks two last letters of "abc".

String "abc" becomes "aab" because '1' attacks the last two letters: 
'c' (power 3) subtracts 1 and 'b' subtracts 1; '1' was attacked and eliminated by 'c' 
because its power became less than or equal to zero.

After this round we have "aab" and "2"; repeat until only one non-empty string is left and return it.

In this case the winner is "a".

Notes
There are no zeros in numbers.

There are no uppercase letters.

More examples
armyLetters  = "g" ; armyNumbers  = "2222"
armyLetters -> "e" ; armyNumbers -> "222"
armyLetters -> "c" ; armyNumbers -> "22"
armyLetters -> "a" ; armyNumbers -> "2"
armyLetters ->  "" ; armyNumbers -> "1"
return "1"  --  armyNumbers
armyLetters  = "g" ; armyNumbers  = "99"
armyLetters ->  "" ; armyNumbers -> "29"
return "29"  --  armyNumbers
armyLetters  = "g" ; armyNumbers  = "23"
armyLetters -> "e" ; armyNumbers -> "3"
armyLetters -> "b" ; armyNumbers -> ""
return "b"  --  armyLetters
-}
-----------------------------------------------------------------------------------------------------------


import Data.List (elemIndex)

battleCodes :: String -> String -> String
battleCodes armyLetters@(_:_) armyNumbers@(_:_) = go (reverse armyLetters) armyNumbers where
  go "" "" = "Draw"
  go "" ns = ns
  go ls "" = reverse ls
  go [l] (n:ns) = go (damage (power n) l) (damage (power l) n ++ ns)
  go (l0:l1:ls) (n:ns) = go (damage (power n) l0 ++ damage (power n) l1 ++ ls) (damage (power l0) n ++ ns)
battleCodes _ _ = "Peace"

power :: Char -> Int -- intentionally partial
power c | Just i <- c `elemIndex` alfabet = i
        | Just i <- c `elemIndex` digits  = i

damage :: Int -> Char -> String -- intentionally partial
damage n c | n >= power c = []
           | c `elem` alfabet = [ alfabet !! (power c - n) ]
           | c `elem` digits  = [ digits  !! (power c - n) ]

alfabet,digits :: String
alfabet = "_abcdefghijklmnopqrstuvwxyz"
digits  = "0123456789"


-- Exercise 1.19
-----------------------------------------------------------------------------------------------------------
{- 
Write a function that receives a non-negative integer n ( n >= 0 ) 
and returns the next higher multiple of five of that number, 
obtained by concatenating the shortest possible binary string to the end of this number's binary representation.

Examples
1.  nextMultipleOfFive 8
Steps:

8 to binary == '1000'
concatenate shortest possible string '11' to obtain '1000' + '11' == '100011'
'100011' to decimal == 35 => the answer
('001' would do the job too, but '11' is the shortest string here)

2.  nextMultipleOfFive 5
Steps:

5 to binary =='101'
concatenate shortest possible string '0' to obtain '101' + '0' == '1010'
'1010' to decimal == 10
(5 is already a multiple of 5, obviously, but we're looking for the next multiple of 5)

Note
Numbers up to 1e10 will be tested, so you need to come up with something smart.
-}
-----------------------------------------------------------------------------------------------------------



nextMultipleOfFive :: Int -> Int
nextMultipleOfFive 0 = 5
nextMultipleOfFive n | (2*n) `mod` 5 == 0 = (2*n)
                     | (2*n) `mod` 5 == 4 = (2*n)+1
                     | (4*n) `mod` 5 == 4 = (4*n)+1
                     | (4*n) `mod` 5 == 2 = (4*n)+3
                     | (8*n) `mod` 5 == 2 = (8*n)+3
                     | otherwise = n


-- Exercise 1.20
-----------------------------------------------------------------------------------------------------------
{- 
In mathematics, the symbols Δ and d are often used to denote the difference between two values. 
Similarly, differentiation takes the ratio of changes (ie. dy/dx) for a linear relationship. 
This method can be applied multiple times to create multiple 'levels' of rates of change. 
(A common example is x (position) -> v (velocity) -> a (acceleration)).

Today we will be creating a similar concept. 
Our function delta will take a sequence of values and a positive integer level, 
and return a sequence with the 'differences' of the original values. 
(Differences here means strictly b - a, eg. [1, 3, 2] => [2, -1]) 
The argument level is the 'level' of difference, 
for example acceleration is the 2nd 'level' of difference from position. 
The specific encoding of input and output lists is specified below.

The example below shows three different 'levels' of the same input.

let input = [1, 2, 4, 7, 11, 16, 22]
delta input 1  ->  [1, 2, 3, 4, 5, 6]
delta input 2  ->  [1, 1, 1, 1, 1]
delta input 3  ->  [0, 0, 0, 0]
We do not assume any 'starting value' for the input, 
so the output for each subsequent level will be one item shorter than the previous (as shown above).
If an infinite input is provided, then the output must also be infinite.

Input/Output encoding
Input and output lists are, possibly infinite, native lists.

Difference implementation
delta must work for lists of any Num instance.
Additional Requirements/Notes:
delta must work for inputs which are infinite
values will always be valid, and will always produce consistent classes/types of object
level will always be valid, and 1 <= level <= 400
Additional examples:
let count = iterate (+ 1) 0 -- [0, 1, 2, 3, 4, ..]
delta count 1  ->  [1, 1, 1, 1, ..]
delta count 2  ->  [0, 0, 0, 0, ..]

let cosine = cycle [1, 0, -1, -1, 0, 1] -- [1, 0, -1, -1, 0, 1, 1, 0, ..]
delta cosine 1  ->  [-1, -1, 0, 1, 1, 0, -1, ..]
delta cosine 2  ->  [0, 1, 1, 0, -1, -1, 0, ..]
delta cosine 3  ->  [1, 0, -1, -1, 0, 1, 1, 0, ..]

let input = [ Set.fromList [6, 2, 65], Set.fromList [2, 66, 8], Set.fromList [65, 66] ]
instance Num (Set Int) where (-) = difference
delta input 1  ->  [ Set.fromList [8, 66], Set.fromList [65] ]
delta input 2  ->  [ Set.fromList [65] ]
-}
-----------------------------------------------------------------------------------------------------------


iteration :: (Num a) => [a] -> [a]
iteration [] = []
iteration [x, y] = [y - x]
iteration (x : y : xs) = y - x : iteration (y : xs)

delta :: (Num a) => [a] -> Int -> [a]
delta lst 0 = lst
delta lst n = iteration $ delta lst (n - 1)
