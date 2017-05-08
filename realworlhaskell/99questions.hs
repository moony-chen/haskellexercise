--P 1
myLast::[a]->a
myLast (x:[]) = x
myLast (x:xs) = myLast xs

--P 2
myButLast::[a]->a
myButLast (x:y:[]) = x
myButLast (x:xs) = myButLast xs