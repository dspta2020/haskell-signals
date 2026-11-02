-- say I have seed some list here 
x = [1,2,3,4,5]

-- this will have all the values in the seed multiplied by 2 
a = [n * 2 | n <- x]

-- now using guards can be convenient for list comprehension 
b = [n * 2 | n <- x, n > 2]

-- now can do that over multiple lists
c = [(i * 2, j) | i <- x, j <- ["a", "b", "c"], i <= 2]           -- guard conditional from one list
d = [(i * 2, j) | i <- x, j <- ["a", "b", "c"], i <= 2, j == "a"] -- guards conditional on both lists
e = [(i * 2, j) | i <- x, j <- ["a", "b", "c"], i <= 2, j == "q"] -- a guard that will never be evaluated true....

