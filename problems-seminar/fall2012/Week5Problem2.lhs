This code computes the answer for Week 5 Problem 2:
Find the number of elements of the largest subset of {1,2,...15} 
such that the product of any three distinct elements of the subset 
is not a square.

> module Main where
> import Data.List
> import Data.Function
> list = [1,2..15]

First, we need a way to generate the subsets of list.
To do this, we need to implement the powerset function.

> -- | The function powerset returns the powerset of xs
> powerset::[a]->[[a]]
> powerset = powerset' [[]]

We will construct the powerset of list recursively,
every iteration adding more subsets of result, starting with result 
just containing the empty set. We will construct these subsets 
by removing elements one by one from our list.

>   where

If we run out of elements in our list to add to result,
then our algorithm has finished and we can return result. 

>     powerset' result []     = result

Otherwise, we take an x from our list, and for every subset in result,
we append x to the subset. We add these new subsets to our collection
in result and run powerset' on our new result and the rest of the list.

>     powerset' result (x:xs) = let result' = result ++ map (x:) result
>                               in powerset' result' xs

We know powerset and powerset' returns the powerset of list.

1. Since on each iteration, we double the number of elements in result,
and we iterate once for each element in our list, so we get 2^n elements in the final result.

2. Visibly, each element in xs appears at most once in each subset in result.

3. Given a subset x_n,x_n-1...x_2,x_1 of list
   (for the sake of our example, we'll assume 
    x_i appears before x_i+1 in the list).
   We know the subset {x_1} is in the powerset, as {} is in result, 
   thus on x_1's iteration, x_1:{}={x_1} is in result'.
   We know the subset {x_2,x_1} is in the powerset, as we just 
   established {x_1} is in result during x_2's iteration,
   so {x_2,x_1} is in result'.
   We can repeat this process, all the way up to x_n.
   So all subsets of list are in powerset list.

Now, we need a way to filter out any subsets with
three distinct elements whose product is a square.

> noSquareProducts::Integral a=>[[a]]->[[a]]
> noSquareProducts xs = filter noSquareProduct xs

For each set in xs, we then check each triplet of distinct elements
in the set.

>  where
>    noSquareProduct set = all (\x->all 
>                                (\y->all 
>                                  (\z->
>                                    x==y || x==z || y==z

We then compute the product and check to see if the product is 
equal to the square root of product floored and squared.
If it is, that the product must be a perfect square.

>                                    || let product = x * y * z
>                                       in product /= (floor (sqrt (fromIntegral product::Double)))^2) 
>                                   set) 
>                                 set) 
>                               set

Next, we need a way to compute which set is the longest.
This we can simply do by comparing their lengths.

> longestSubset::[[a]]->[a]
> longestSubset xs = maximumBy (compare `on` length) xs

Then we combine the three functions we have constructed to
find the answer to our problem.

> main = print $ longestSubset . 
>                noSquareProducts . 
>                powerset $ list
