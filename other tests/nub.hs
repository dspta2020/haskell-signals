input_list = ["a", "a", "b", "a", "c", "d", "g", "g", "t", "b"]
int_list_asc_ordered = [1,2,3,4,5] :: [Int]
int_list_not_asc = [3,2,2,1,5] :: [Int]
int_list_asc_not_ordered = [1,2,2,3,5] :: [Int]

-- function that evaluates if a value is in a list 
is_element_of :: (Eq a) => a -> [a] -> Bool
-- case here the list is empty can never be true
is_element_of _ [] = False
-- if that element then return true or search recursive until you find the element or exhaust the list
is_element_of e (x:xs) = (e == x) || (e `is_element_of` xs) 

-- function that removes all duplicates from a list
unique :: (Eq a) => [a] -> [a]
unique [] = []
unique (e:xs)
  | e `is_element_of` xs = unique xs     -- if e is an element of xs then continue deconstructing xs
  | otherwise            = e : unique xs -- if e is not in the rest of the list it's not a duplicate

-- function that evaluates if the list is ascending
isAsc :: [Int] -> Bool
isAsc [] = True 
isAsc [x] = True 
-- pretty self explanatory evaluates the first expression first (e.g. if consecutive elements are not ascending) then 
-- if they are not ascending then the `isAsc` is not evaluated
isAsc (x:y:xs) = (x <= y) && isAsc (y:xs)