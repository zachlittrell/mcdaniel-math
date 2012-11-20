module Main where
import qualified Data.List as List


-- | The 'permutations' function returns all maps between
-- xs and itself.
permutations::[a]->[[(a,a)]]
permutations xs = map (zip xs) (List.permutations xs)

largeIntegers::Ord a=>[(a,a)]->[a]
largeIntegers []     = []
largeIntegers ((x,y):xs) = if all ((y >).snd) xs
                           then y:largeIntegers xs
                           else largeIntegers xs
