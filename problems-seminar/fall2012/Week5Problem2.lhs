This code computes the answer for Week 5 Problem 2:
Find the number of elements of the largest subset of {1,2,...15} 
such that the product of any three distinct elements of the subset 
is not a square.

> module Main where
> import Data.Bits
> import Data.Function
> import Data.List
> import Data.Maybe
> list = [1,2..15]

First, we need a way to generate the subsets of list.
To do this, we need to implement the powerset function.

> -- | The function powerset returns the powerset of xs
> powerset::([a]->Bool)->[a]->[[a]]
> powerset f xs = [set | n<-[0..((2^len)::Integer)],

We can actually map each subset to a binary number between
0 and 2^100, such that each power of 2, i, maps to the
set containing {i}.

>                       let set = setFromBits n,

As soon as we generate the subset, we can check to see if it
passes our tests. If it doesn't, then we can safely toss it.

>                       f set]
>   where
>     len::Integer
>     len = genericLength xs

We need to be able to check to see what bits are 'on', thus
which elements are in our subset. This we can trivially do 
by seeing if when we look at the intersection of n's bits
and 2^i, if there is at least one bit on (namely, the bit
at the ith position)

>     bitSet n i = n .&. (2^i) /= 0
>     setFromBits n = [xs `genericIndex` i | i<-[0..(len-1)],bitSet n i]
>     subsetMaybe n = let set = setFromBits n
>                     in if (f set)
>                        then Just set
>                        else Nothing
                    

Now, we need a way to filter out any subsets with
three distinct elements whose product is a square.

For each set in xs, we then check each triplet of distinct elements
in the set.

> noSquareProducts set = all (\x-> all
>                              (\y->x==y || all
>                                (\z->
>                                   x==z || y==z

We then compute the product and check to see if the product is 
equal to the square root of product floored and squared.
If it is, that the product must be a perfect square.

>                                  || let product = x * y * z
>                                     in product /= (floor (sqrt (fromIntegral product::Double)))^2) 
>                                 set) 
>                               set) 
>                             set

Next, we need a way to compute which set is the longest.
This we can simply do by comparing their lengths.

> longestSubset::[[a]]->[a]
> longestSubset xs = maximumBy (compare `on` length) xs

Then we combine the three functions we have constructed to
find the answer to our problem.

> main = print $ longestSubset (powerset noSquareProducts list)
