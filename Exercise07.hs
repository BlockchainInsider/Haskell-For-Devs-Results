module Exercise07 where


-- Exercise 7.1
-----------------------------------------------------------------------------------------------------------
{- 
Consider a sequence generation that follows the following steps. We will store removed values in variable res. 
Assume n = 25:

-> [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25] 
Let's remove the first number => res = [1]. We get..
-> [2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25]. 
Let's remove 2 (so res = [1,2]) and every 2-indexed number. We get..
-> [3,5,7,9,11,13,15,17,19,21,23,25]. Now remove 3, then every 3-indexed number. res = [1,2,3]. 
-> [5,7,11,13,17,19,23,25]. Now remove 5, and every 5-indexed number. res = [1,2,3,5]. We get..
-> [7,11,13,17,23,25]. Now remove 7 and every 7-indexed. res = [1,2,3,5,7].  
But we know that there are no other 7-indexed elements, so we include all remaining numbers in res. 
So res = [1,2,3,5,7,11,13,17,23,25] and sum(res) = 107.

Note that when we remove every n-indexed number, we must remember that indices start at 0. 
So for every 3-indexed number above:
[3,5,7,9,11,13,15,17,19,21,23], we remove index0=3, index3= 9, index6=15,index9=21, etc.

Note also that if the length of sequence is greater than x, 
where x is the first element of the sequence, you should continue the remove step: remove x, 
and every x-indexed number until the length of sequence is shorter than x. In our example above, 
we stopped at 7 because the the length of the remaining sequence [7,11,13,17,23,25] is shorter than 7.
You will be given a number n and your task will be to return the sum of the elements in res, 
where the maximum element in res is <= n.

For example:

Solve(7) = 18, because this is the sum of res = [1,2,3,5,7].
Solve(25) = 107
-}
-----------------------------------------------------------------------------------------------------------


import Data.Maybe
import Control.Monad

solve :: Int -> Int
solve n = 1 + solve' [2..n] where
  solve' [] = 0
  solve' (x:xs) = x + solve' (mapMaybe filterFunc $ zip [1..] xs) where
    filterFunc (i,a) = guard (i `mod` x > 0) >> return a


-- Exercise 7.2
-----------------------------------------------------------------------------------------------------------
{- 
A rectangle can be split up into a grid of 1x1 squares, 
the amount of which being equal to the product of the two dimensions of the rectangle. 
Depending on the size of the rectangle, that grid of 1x1 squares can also be split up into larger squares, 
for example a 3x2 rectangle has a total of 8 squares, as there are 6 distinct 1x1 squares, and two possible 2x2 squares. 
A 4x3 rectangle contains 20 squares.

Your task is to write a function `findSquares` that returns the total number of squares for any given rectangle, 
the dimensions of which being given as two integers with the first always being equal to or greater than the second.
-}
-----------------------------------------------------------------------------------------------------------


findSquares :: Int -> Int -> Int
findSquares 0 _ = 0
findSquares _ 0 = 0
findSquares x y = x * y + findSquares (x - 1) (y - 1)


-- Exercise 7.3
-----------------------------------------------------------------------------------------------------------
{-
FizzBuzz is often one of the first programming puzzles people learn. Now undo it with reverse FizzBuzz!

Write a function that accepts a string, which will always be a valid section of FizzBuzz. 
Your function must return an array that contains the numbers in order to generate the given section of FizzBuzz.

Notes:

If the sequence can appear multiple times within FizzBuzz, 
return the numbers that generate the first instance of that sequence.
All numbers in the sequence will be greater than zero.
You will never receive an empty sequence.
Examples
reverse_fizzbuzz("1 2 Fizz 4 Buzz")        -->  [1, 2, 3, 4, 5]
reverse_fizzbuzz("Fizz 688 689 FizzBuzz")  -->  [687, 688, 689, 690]
reverse_fizzbuzz("Fizz Buzz")              -->  [9, 10]
-}
-----------------------------------------------------------------------------------------------------------


import           Data.Char                      ( isDigit )

reverseFizzBuzz :: String -> [Int]
reverseFizzBuzz str = case xs of
    ["Fizz"    ]     -> [3]
    ["Buzz"    ]     -> [5]
    ["FizzBuzz"]     -> [15]
    ["Fizz", "Buzz"] -> [9, 10]
    ["Buzz", "Fizz"] -> [5, 6]
    _                -> take (length xs) [n - idx ..]
  where
    xs       = words str
    (n, idx) = findNumber 0 xs

findNumber :: Int -> [String] -> (Int, Int)
findNumber n [] = undefined
findNumber n (x : xs) | all isDigit x = (read x, n)
                      | otherwise     = findNumber (n + 1) xs


-- Exercise 7.4
-----------------------------------------------------------------------------------------------------------
{- 
Given three arrays of integers, return the sum of elements that are common in all three arrays.

For example:

common([1,2,3],[5,3,2],[7,3,2]) = 5 because 2 & 3 are common in all 3 arrays
common([1,2,2,3],[5,3,2,2],[7,3,2,2]) = 7 because 2,2 & 3 are common in the 3 arrays
More examples in the test cases.
-}
-----------------------------------------------------------------------------------------------------------


import qualified Data.IntMap.Strict as Q

counter :: [Int] -> Q.IntMap Int
counter !xs = Q.fromListWith (+) $ map (,1) xs

infixl 9 `intersect`
intersect :: Q.IntMap Int -> Q.IntMap Int -> Q.IntMap Int
intersect !im1 !im2 = Q.intersectionWith min im1 im2

total :: Q.IntMap Int -> Int
total !im = Q.foldlWithKey (\s i x -> s + i*x) 0 im

common :: [Int] -> [Int] -> [Int] -> Int
common !xs !ys !zs = total $ counter xs `intersect` counter ys `intersect` counter zs


-- Exercise 7.5
-----------------------------------------------------------------------------------------------------------
{- 
Can a value be both True and False?

Define OmniBool so that it returns True for the following:

omniBool == True && omniBool == False
-}
-----------------------------------------------------------------------------------------------------------


data OmniBool = OmniBool

(==) :: OmniBool -> Bool -> Bool
OmniBool == _ = True

omniBool :: OmniBool
omniBool = OmniBool


-- Exercise 7.6
-----------------------------------------------------------------------------------------------------------
{- 
Consider an array containing cats and dogs. Each dog can catch only one cat, 
but cannot catch a cat that is more than n elements away. 
Your task will be to return the maximum number of cats that can be caught.

For example:

solve(['D','C','C','D','C'], 2) = 2, because the dog at index 0 (D0) catches C1 and D3 catches C4. 
solve(['C','C','D','D','C','D'], 2) = 3, because D2 catches C0, D3 catches C1 and D5 catches C4.
solve(['C','C','D','D','C','D'], 1) = 2, because D2 catches C1, D3 catches C4. C0 cannot be caught because n == 1.
solve(['D','C','D','D','C'], 1) = 2, too many dogs, so all cats get caught!
Do not modify the input array.
-}
-----------------------------------------------------------------------------------------------------------


import Data.Char
import Data.Maybe

solve :: [Char] -> Int -> Int
solve a n = f a n 0 []
            where f a n i o | i == length a = length o
                            | x == Nothing = f a n (i+1) o
                            | otherwise = f a n (i+1) (o++[fromJust x])
                              where x = g a n i o

g :: String -> Int -> Int -> [Int] -> Maybe Int
g a n i o | a!!i /= 'D' || null cat = Nothing
          | otherwise = Just $ head cat
            where
            r = [(maximum [0,i-n])..(minimum [(length a) - 1, i + n])]
            cat = filter(\j -> a!!j == 'C' && notElem j o) r


-- Exercise 7.7
-----------------------------------------------------------------------------------------------------------
{-
 Write the following function:

areaOfPolygonInsideCircle :: Double -> Int -> Double
areaOfPolygonInsideCircle circleRadius numberOfSides = undefined
It should calculate the area of a regular polygon of numberOfSides, number-of-sides, 
or number_of_sides sides inside a circle of radius circleRadius, circle-radius, 
or circle_radius which passes through all the vertices of the polygon 
(such circle is called circumscribed circle or circumcircle). 
The answer should be a number rounded to 3 decimal places.

Input :: Output Examples

areaOfPolygonInsideCircle 3 3 -- returns 11.691

areaOfPolygonInsideCircle 5.8 7 -- returns 92.053

areaOfPolygonInsideCircle 4 5 -- returns 38.042
Note: if you need to use Pi in your code, use the native value of your language unless stated otherwise.
-}
-----------------------------------------------------------------------------------------------------------


import Control.Arrow((>>>))

areaOfPolygonInsideCircle :: Double -> Int -> Double
areaOfPolygonInsideCircle circleRadius (fromIntegral -> numberOfSides) =
    precision3places $ areaOfTriangle * numberOfSides
  where
    precision3places = (*1000) >>> round >>> fromIntegral >>> (/1000)
    areaOfTriangle = circleRadius**2 / 2 * sin angle
    angle = pi * (numberOfSides  - 2) / numberOfSides


-- Exercise 7.8
-----------------------------------------------------------------------------------------------------------
{- 
In the world of birding there are four-letter codes for the common names of birds. 
These codes are created by some simple rules:

If the bird's name has only one word, the code takes the first four letters of that word.
If the name is made up of two words, the code takes the first two letters of each word.
If the name is made up of three words, 
the code is created by taking the first letter from the first two words and the first two letters from the third word.
If the name is four words long, the code uses the first letter from all the words.
(There are other ways that codes are created, but this Kata will only use the four rules listed above)

Complete the function that takes an array of strings of common bird names from North America, 
and create the codes for those names based on the rules above. 
The function should return an array of the codes in the same order in which the input names were presented.

Additional considertations:

The four-letter codes in the returned array should be in UPPER CASE.
If a common name has a hyphen/dash, it should be considered a space.
Example
If the input array is: ["Black-Capped Chickadee", "Common Tern"]

The return array would be: ["BCCH", "COTE"]
-}
-----------------------------------------------------------------------------------------------------------


import Data.Char (toUpper)
import Data.List.Split (splitOn)

birdCode :: [String] -> [String]
birdCode = map (map toUpper . take 4 . toCode . toWords)

toCode :: [String] -> String
toCode xs
  | length xs >= 4 = concatMap (take 1) xs
  | length xs == 3 = concat . (: [last xs]) . map head . init $ xs
  | length xs == 2 = concatMap (take 2) xs
  | otherwise = head xs

toWords :: String -> [String]
toWords = concatMap (splitOn "-") . words


-- Exercise 7.9
-----------------------------------------------------------------------------------------------------------
{- 
you will need to return a boolean value if the base string can be expressed as the repetition of one subpattern.

This time there are two small changes:

if a subpattern has been used, it will be present at least twice, 
meaning the subpattern has to be shorter than the original string;
the strings you will be given might or might not be created repeating a given subpattern, 
then shuffling the result.
For example:

hasSubpattern("a") == false; //no repeated shorter sub-pattern, just one character
hasSubpattern("aaaa") == true; //just one character repeated
hasSubpattern("abcd") == false; //no repetitions
hasSubpattern("babababababababa") == true; //repeated "ba"
hasSubpattern("bbabbaaabbaaaabb") == true; //same as above, just shuffled
Strings will never be empty and can be composed of any character 
(just consider upper- and lowercase letters as different entities) 
and can be pretty long (keep an eye on performances!).
-}
-----------------------------------------------------------------------------------------------------------


import Data.List

hasSubpattern :: String -> Bool
hasSubpattern xs = length zs == 1 && (head zs >= 2)
    where zs = (nub . map length . group . sort) xs


-- Exercise 7.10
-----------------------------------------------------------------------------------------------------------
{- 
A natural number is called k-prime if it has exactly k prime factors, counted with multiplicity.

A natural number is thus prime if and only if it is 1-prime.

Examples of k-primes:
k = 2 -> 4, 6, 9, 10, 14, 15, 21, 22, …
k = 3 -> 8, 12, 18, 20, 27, 28, 30, …
k = 5 -> 32, 48, 72, 80, 108, 112, …
The k-prime numbers are not regularly spaced. For example: between 2 and 50 we have the following 2-primes:

[4, 6, 9, 10, 14, 15, 21, 22, 25, 26, 33, 34, 35, 38, 39, 46, 49]

The steps between two k-primes of this list are 2, 3, 1, 4, 1, 6, 1, 3, 1, 7, 1, 1, 3, 1, 7, 3.

Task
We will write a function kprimes_step(k, step, start, nd) with parameters:

k (integer > 0) which indicates the type of k-primes we are looking for,

step (integer > 0) which indicates the step we want to find between two k-primes,

start (integer >= 0) which gives the start of the search (start inclusive),

nd (integer >= start) which gives the end of the search (nd inclusive)

In the example above kprimes_step(2, 2, 0, 50) will return  
[[4, 6], [33, 35]] which are the pairs of 2-primes between 2 and 50 with a 2 steps.

So this function should return an array of all the pairs 
(or tuples) of k-prime numbers spaced with a step of step between the limits start, nd. 
This array can be empty.

Note (Swift)
When there is no pair instead of returning an empty array, return nil

kprimes_step(5, 20, 0, 50) => nil
Examples:
kprimes_step(2, 2, 0, 50) => [[4, 6], [33, 35]]
kprimes_step(6, 14, 2113665, 2113889) => [[2113722, 2113736]])
kprimes_step(2, 10, 0, 50) => [[4, 14], [15, 25], [25, 35], [39, 49]]
kprimes_step(5, 20, 0, 50) => []
-}
-----------------------------------------------------------------------------------------------------------


prFactors :: Int -> Int -> Bool
prFactors nb k = (length $ prFactors' nb 2) ==  k
  where
    prFactors' nb fact
      | fact * fact > nb    = [nb]
      | nb `mod` fact == 0  = fact : prFactors' (nb `div` fact) fact
      | otherwise           = prFactors' nb (fact + 1)
kprimesStep :: Int -> Int -> Int -> Int -> Maybe [(Int, Int)]
kprimesStep k step m n = res
        where
            r = [(p, p + step) | p <- [m..n - step], (prFactors p k == True) && (prFactors (p + step) k == True)]
            res 
                | r /= [] = Just r
                | otherwise = Nothing


-- Exercise 7.11
-----------------------------------------------------------------------------------------------------------
{- 
Given a string, return the minimal number of parenthesis reversals needed to make balanced parenthesis.

For example:

solve(")(") = 2 Because we need to reverse ")" to "(" and "(" to ")". These are 2 reversals. 
solve("(((())") = 1 We need to reverse just one "(" parenthesis to make it balanced.
solve("(((") = -1 Not possible to form balanced parenthesis. Return -1.
Parenthesis will be either "(" or ")".
-}
-----------------------------------------------------------------------------------------------------------


import Data.List

solve :: String -> Int 
solve xs = solve' xs 0
             where
               solve' [] a = a
               solve' xs a
                 | odd $ length xs            = -1
                 | head xs == ')'             = solve' ('(':(tail xs)) (a+1)
                 | last xs == '('             = solve' ((init xs) ++ [')']) (a+1)
                 | elem '(' xs && elem ')' xs = solve' (delete '(' $ delete ')' $ xs) a
                 | otherwise                  = a + length xs `div` 2


-- Exercise 7.12
-----------------------------------------------------------------------------------------------------------
{- 
You will be given a string and you task is to check if it is possible to 
convert that string into a palindrome by removing a single character. 
If the string is already a palindrome, return "OK". 
If it is not, and we can convert it to a palindrome by removing one character, 
then return "remove one", otherwise return "not possible". 
The order of the characters should not be changed.

For example:

solve("abba") = "OK". -- This is a palindrome
solve("abbaa") = "remove one". -- remove the 'a' at the extreme right. 
solve("abbaab") = "not possible". 
-}
-----------------------------------------------------------------------------------------------------------


data Palindrome = Pure | Impure | Impossible deriving Eq

solve :: String -> String
solve str = case palindrome Pure str of
  Pure -> "OK"
  Impure -> "remove one"
  Impossible -> "not possible"

palindrome :: Palindrome -> String -> Palindrome
palindrome p [] = p
palindrome p (_:[]) = p
palindrome p str
  | head str == last str = palindrome p . init . tail $ str
  | p == Pure = case palindrome Impure (init str) of
    Impossible -> palindrome Impure (tail str)
    Impure -> Impure
  | otherwise = Impossible


-- Exercise 7.13
-----------------------------------------------------------------------------------------------------------
{- 
In this Kata, you will be given a list of strings 
and your task will be to find the strings that have the same characters 
and return the sum of their positions as follows:

solve(["abc","abbc", "ab", "xyz", "xy", "zzyx"]) = [1,8]
-- we see that the elements at indices 0 and 1 have the same characters, as do those at indices 3 and 5.
-- we therefore return [1,8] because [0+1,3+5] = [1,8]. This result is sorted. 
-- ignore those that don't have at least one matching partner, such as "ab" and "xy".

Another example...
solve(["wkskkkkkk","fokoo","wkskk","uizzzz","fokooff","wkskkkk","uizzzzzzz"]),[5,7,9]);
--The element at index 0 is similar to those at indices 2 and 5; so 0 + 2 + 5 = 7.
--The element at index 1 is similar to that at index 4; so 1 + 4 = 5. 
--The element at index 3 is similar to that at index 6; so 3 + 6 = 9.
--The result must be sorted. We get [5,7,9].
-}
-----------------------------------------------------------------------------------------------------------


import           Control.Arrow
import           Data.List     (sort)
import qualified Data.Map      as Map
import qualified Data.Set      as Set

solve :: [String] -> [Int]
solve =
  map Set.fromList
    >>> flip zip (pure <$> [0 ..])
    >>> Map.fromListWith (++)
    >>> Map.elems
    >>> filter ((> 1) . length)
    >>> map sum
    >>> sort


-- Exercise 7.14
-----------------------------------------------------------------------------------------------------------
{- 
two players, Alice and Bob, are playing a palindrome game. 
Alice starts with string1, Bob starts with string2, and the board starts out as an empty string. 
Alice and Bob take turns; during a turn, a player selects a letter from his or her string, 
removes it from the string, and appends it to the board; if the board becomes a palindrome (of length >= 2), 
the player wins. Alice makes the first move. Since Bob has the disadvantage of playing second, 
then he wins automatically if letters run out or the board is never a palindrome. 
Note also that each player can see the other player's letters.

The problem will be presented as solve(string1,string2). Return 1 if Alice wins and 2 it Bob wins.

For example:

solve("abc","baxy") = 2 -- There is no way for Alice to win. 
If she starts with 'a', Bob wins by playing 'a'. 
The same case with 'b'. If Alice starts with 'c', 
Bob still wins because a palindrome is not possible. Return 2.
solve("eyfjy","ooigvo") = 1 -- Alice plays 'y' and whatever Bob plays, 
Alice wins by playing another 'y'. Return 1.
solve("abc","xyz") = 2 -- No palindrome is possible, so Bob wins; return 2
solve("gzyqsczkctutjves","hpaqrfwkdntfwnvgs") = 1 
-- If Alice plays 'g', Bob wins by playing 'g'. Alice must be clever. 
She starts with 'z'. She knows that since she has two 'z', the win is guaranteed. 
Note that she also has two 's'. But she cannot play that. Can you see why? 
solve("rmevmtw","uavtyft") = 1 -- Alice wins by playing 'm'. Can you see why? 
Palindrome lengths should be at least 2 characters. More examples in the test cases.
-}
-----------------------------------------------------------------------------------------------------------


import Data.List

solve :: String -> String -> Int
solve xs ys
  | null amy = 2
  | intersect amy ys == amy = 2
  | otherwise = 1
  where amy = concat $ filter (\x -> length x > 1) $ group $ sort xs


-- Exercise 7.15
-----------------------------------------------------------------------------------------------------------
{- 
you will be given a string with brackets and an index of an opening bracket 
and your task will be to return the index of the matching closing bracket. 
Both the input and returned index are 0-based except in Fortran where it is 1-based. 
An opening brace will always have a closing brace. Return -1 if there is no answer (in Haskell, return Nothing; 
in Fortran, return 0; in Go, return an error)

Examples
solve("((1)23(45))(aB)", 0) = Just 10 -- the opening brace at index 0 matches the closing brace at index 10
solve("((1)23(45))(aB)", 2) = Nothing -- there is no opening bracket at index 2, so return "Nothing" instead of -1
Input will consist of letters, numbers and special characters, but no spaces. The only brackets will be ( and ).
-}
-----------------------------------------------------------------------------------------------------------


solve :: String -> Int -> Maybe Int
solve str idx = lookup idx $ matchingParens str
  where
    matchingParens :: String -> [(Int, Int)]
    matchingParens = go 0 []
      where
        go _ _ [] = []
        go index open (c:cs) =
          case c of
            '(' -> go (index + 1) (index : open) cs
            ')' ->
              case open of
                (o:os) -> (o, index) : go (index + 1) os cs
                _ -> error ""
            _ -> go (index + 1) open cs


-- Exercise 7.16
-----------------------------------------------------------------------------------------------------------
{- 
you will be given a string containing numbers from a to b, one number can be missing from these numbers, 
then the string will be shuffled, you're expected to return an array of all possible missing numbers.

Examples (input => output)
Here's a string with numbers from 1 - 21, its missing one number and the string is then shuffled, 
your'e expected to return a list of possible missing numbers.

1, 21, "2198765123416171890101112131415"  => [ 12, 21 ]
You won't be able to tell if its 21 or 12, you must return all possible values in an array.

Another Example
5, 10, "578910" => [ 6 ]
Documentation:
The parameters will be in order two numbers and a string:

start => from
stop => to
string => numbers from start to stop in string form (shuffled), but missing one number
Note:

if there're no missing numbers return an empty list
-}
-----------------------------------------------------------------------------------------------------------


import Data.List

convertStrToInt :: String -> Int
convertStrToInt s = read s :: Int

findNumber :: Int -> Int -> [Char] -> Maybe [Int]
findNumber init end str =
  let orig = (sort . concatMap show) [init .. end]
      input = sort str
      diff = orig \\ input
   in case diff of
        [] -> Nothing
        _ -> (Just . nub . filter (\x -> x > init && x <= end) . map convertStrToInt . permutations) diff



-- Exercise 7.17
-----------------------------------------------------------------------------------------------------------
{- 
You are given an array of integers. 
Your task is to sort odd numbers within the array in ascending order, and even numbers in descending order.

Note that zero is an even number. If you have an empty array, you need to return it.

For example:

[5, 3, 2, 8, 1, 4]  -->  [1, 3, 8, 4, 5, 2]

odd numbers ascending:   [1, 3,       5   ]
even numbers descending: [      8, 4,    2]
-}
-----------------------------------------------------------------------------------------------------------


import Data.List (sort, sortOn)
import Data.Ord (Down (Down))

paritySort :: [Int] -> [Int]
paritySort xs = map fst . sortOn snd $ evens ++ odds
  where
    indexed = zip xs [0 ..] :: [(Int, Int)]
    evens = sortEvens . filter (even . fst) $ indexed
    odds = sortOdds . filter (odd . fst) $ indexed

sortEvens :: [(Int, Int)] -> [(Int, Int)]
sortEvens xs = zip values indices
  where
    indices = map snd xs
    values = sortOn Down . map fst $ xs

sortOdds :: [(Int, Int)] -> [(Int, Int)]
sortOdds xs = zip values indices
  where
    indices = map snd xs
    values = sort . map fst $ xs


-- Exercise 7.18
-----------------------------------------------------------------------------------------------------------
{- 
In this Kata, we are going to determine 
if the count of each of the characters in a string can be equal 
if we remove a single character from that string.

For example:

solve('abba') = false -- if we remove any character, the count of each character will not be equal.
solve('abbba') = true -- if we remove one b, the count of each character becomes 2.
solve('aaaa') = true -- if we remove one character, the remaining characters have same count.
solve('wwwf') = true -- if we remove f, the remaining letters have same count.
-}
-----------------------------------------------------------------------------------------------------------


import qualified Data.HashMap.Strict as HMap
import Data.List

solve :: String -> Bool
solve s = elem l [1, length s] || l == 2 && m == 1 || m == 1 && nm == (l-1) || m' == m+1 && nm ==1
        where
        s' = nub s
        l = length s'
        counter = HMap.fromListWith (+) $ zip s (repeat 1)
        vals = map(\k -> counter HMap.! k) s'
        m' = maximum vals
        m = minimum vals
        nm = length $ filter(\x -> (counter HMap.! x) == m') s'


-- Exercise 7.19
-----------------------------------------------------------------------------------------------------------
{- 
you will be given an array and your task will be to determine 
if an array is in ascending or descending order and if it is rotated or not.

Consider the array [1,2,3,4,5,7,12]. This array is sorted in Ascending order. 
If we rotate this array once to the left, we get [12,1,2,3,4,5,7] 
and twice-rotated we get [7,12,1,2,3,4,5]. These two rotated arrays are in Rotated Ascending order.

Similarly, the array [9,6,5,3,1] is in Descending order, 
but we can rotate it to get an array in Rotated Descending order: [1,9,6,5,3] or [3,1,9,6,5] etc.

Arrays will never be unsorted, except for those that are rotated as shown above. 
Arrays will always have an answer, as shown in the examples below. Arrays will never contain duplicated elements.

More examples:

solve([1,2,3,4,5,7]) = "A" -- Ascending
solve([7,1,2,3,4,5]) = "RA" -- Rotated ascending
solve([4,5,6,1,2,3]) = "RA" -- Rotated ascending
solve([9,8,7,6]) = "D" -- Descending
solve([5,9,8,7,6]) = "RD" -- Rotated Descending
-}
-----------------------------------------------------------------------------------------------------------


solve :: [Int] -> String
solve (x:xs) = g $ foldl f ((0, 0), x) xs
    where
        f ((lt, gt), prev) next = if prev < next then ((lt, gt + 1), next) else ((lt + 1, gt), next)
        g ((lt, gt), _)
            | lt == 0   = "A"
            | gt == 0   = "D"
            | lt > gt   = "RD"
            | lt < gt   = "RA"
            | otherwise = if any (< x) (init xs) then "RA" else "RD"


-- Exercise 7.20
-----------------------------------------------------------------------------------------------------------
{- 
A special type of prime is generated by the formula p = 2^m * 3^n + 1 
where m and n can be any non-negative integer.

The first 5 of these primes are 2, 3, 5, 7, 13, and are generated as follows:

2 = 2^0 * 3^0 + 1
3 = 2^1 * 3^0 + 1
5 = 2^2 * 3^0 + 1
7 = 2^1 * 3^1 + 1
13 = 2^2 * 3^1 + 1
..and so on
You will be given a range and your task is to return the number of primes that have this property. 
For example, solve(0,15) = 5, because there are only 5 such primes >= 0 and < 15; they are 2,3,5,7,13. 
The upper limit of the tests will not exceed 1,500,000.
-}
-----------------------------------------------------------------------------------------------------------


solve :: Int -> Int -> Int
solve x y = sum [1 |
                  a <- takeWhile (< y - 1) $ iterate (* 2) 1,
                  b <- takeWhile (< y - 1) $ iterate (* 3) 1,
                  let p = a * b + 1,
                  x <= p, p < y, isPrime p]
  where isPrime :: Int -> Bool
        isPrime n = all ((/= 0) . mod n) [2 .. floor $ sqrt $ fromIntegral n]
