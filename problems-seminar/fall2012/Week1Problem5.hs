module Main where

--f::(Num n, Eq n, Ord n)=>n->[n]
f n = f' 1 [[0]] 
  where
    iteration z prevs = let z2 = z^2
                            next = map (++[z2]) prevs ++
                                   map (++[-z2]) prevs
                        in [next,filter ((== n) . sum) next]
    f' z prevs = let [next,possible] = iteration z prevs
                 in if null possible
                    then f' (z+1) next
                    else possible ++ f'' (z+1) next 0
    f'' z prevs 10 = []
    f'' z prevs n   =  let [next,possible] = iteration z prevs
                       in possible ++ f'' (z+1) next (n+1)
