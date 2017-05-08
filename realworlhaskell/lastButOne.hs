-- lastButOne.hs
lastButOne::[a]->a
lastButOne xs = if null (tail (tail xs))
                then head xs
                else lastButOne (tail xs)