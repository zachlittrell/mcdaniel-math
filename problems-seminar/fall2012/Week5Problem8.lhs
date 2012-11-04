This program generates rows of pascal's triangle that have only odd elements.

>module Main where
>import Data.List

In order to compute choices/pascal triangle rows, it's necessary to first
define factorial.

To cut down on the number of computations we have to perform for finding n!,
we instead have an infinite list containing the results of n!.
To retrieve the value of n!, we can just use factorial!!n.

>factorial::[Integer]
>factorial=scanl (*) 1 [1..]

With factorial defined, we can define choose=n!/(k!(n-k)!).

>choose::Integer->Integer->Integer
>n `choose` k = factorial!!n `div` (factorial!!k * factorial!!(n-k))
>  where
>    (!!) = genericIndex

Now, we can construct oddChoices, which returns all the rows in pascal's triangle that contains only odd elements, by simply applying choose to all n
and k, and filtering out rows which have even terms.

>oddChoices::[(Integer,[Integer])]
>oddChoices = [(n, choices) | n<-[0..],
>                             let choices = map (choose n) [0..n],
>                             all odd choices]

>main = mapM_ print (take maxN oddChoices)
>  where
>    maxN = 5
