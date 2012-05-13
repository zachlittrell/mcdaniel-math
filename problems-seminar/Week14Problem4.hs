module Main where

import Data.List

-- | The 'f' function returns the smallest number n
-- such that it is composed of only the digits a and b,
-- contains at least one of each, and is divisible by both.
-- Note, a should be the smaller digit.
f a b = f' [""]
  where
    --We convert a and b into characters
    digitA = head (show a)
    digitB = head (show b)
    --First, taking the last combinations, in order from least
    --to greatest, called nums, we're going construct a new 
    --pair of combinations, by first appending a and then b.
    --i.e. newNums = {a:num | for each num in nums} U
    --               {b:num | for each num in nums}
    f' nums = let newNums = [digitA:num | num<-nums] ++
                            [digitB:num | num<-nums]
                  --valid Nums is all of the newNums that
                  --contain only digits a and b, at least one of each,
                  --and is divisible by both digits.
                  validNums = [n | num <- newNums,
                                   --we see if digitA and digitB
                                   --are both contained in num.
                                   digitA `elem` num,
                                   digitB `elem` num,
                                   --convert num into an actual
                                   --integer
                                   let n = read num,
                                   --we see if the remainder of
                                   --n/a and n/b is zero
                                   n `rem` a == 0,
                                   n `rem` b == 0]
                 --if there are no validNums, try the next digit place
                 --else, return the least validNum (which is at the
                 --head of the list)
              in if null validNums
                 then f' newNums
                 else head validNums


main = do
         --Find the pair of digits which produces the maximum
         --value from f(a,b)
         let [x,y,xy'] = maximumBy (\[_,_,xy'] [_,_,ab']->
                                     xy' `compare` ab')
                         fs
                         
         putStr "f("
         putStr (show x)
         putStr ","
         putStr (show y)
         putStr ")="
         putStrLn (show xy')
  where
    --fs is all pairs of digits x and y and the result of f(a,b),
    --so long as the pair of digit has a solution, as defined
    --by hasSolution.
    fs = [[x,y, f x y] | x<-[1..9], y<-[x..9], hasSolution x y]
    --returns false iff one digit is 5 and the other is even.
    hasSolution x y
      | x==5 && even y = False
      | y==5 && even x = False
      | otherwise      = True

{-
 - Proof that included implementation of f(a,b) is correct:
 - (note, we don't have to worry about the case where a or b = 0,
 - since visibly f(zero, non-zero) = undefined,
 - and f(0,0) = 0 < f(1,1) = 1)
 - Also, we can exclude the cases where a = 5 and b is even
 - and non-zero (or vice
 - versa), since if the last digit is 5, it is not an even number,
 - and if the number is even, it ends in an even digits, so either
 - it ends with an even digit between 2-8, thus isn't divisible by 5,
 - or ends in a zero, which violates our assumption.
 -
 - Since the first iteration of f'(nums) lets num = [""],
 - newNums = [a:""] ++ [b:""] = [a] ++ [b] == [a,b]
 - Since we're given that a < b, we know newNums is in
 - ascending order, so if any element survives validNum's
 - tests, the head of validNums will be the least element to
 - meet it's tests. Else, we can give this for another
 - iteration of f'
 -
 - Now, let's consider all the following iterations, assuming
 - the last iteration was correct, thus nums is in ascending order.
 - Given that a < b, for any n digit number, d_1d_2...d_n,
 - ad_1d_2...d_n < bd_1d_2...d_n,
 - Also, given another n digit number, e_1e_2...e_n > d_1d_2...d_n,
 - ae_1e_2...e_n > ad_1d_2...d_n.
 - Thus, given our construction of newNums, we know that it is
 - in ascending order, so validNums will be in ascending order,
 - and if any element survives validNums's test, the head of these
 - elements will be f(a,b).  Else, we can give these numbers
 - to another iteration of f'.
 -}
