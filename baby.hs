doubleMe x = x + x

doubleUs x y = doubleMe x + doubleMe y

doubleSmallNumber x =
  if x > 100
    then x
    else x * 2

doubleSmallNumber' x = (if x > 100 then x else x * 2) + 1

conanO'Brien = "It's a-me, Mario"

factorial x = [1 .. x]

removeLowercaseChars str = [c | c <- str, c `elem` ['A' .. 'Z']]

fact 0 = 1
fact x = x * fact (x -1)

maxx :: (Ord a) => [a] -> a
maxx [] = error "max of an empty list attempted"
maxx [x] = x
maxx (x : xs)
  | x > maxTail = x
  | otherwise = maxTail
  where
    maxTail = maxx xs

minn :: (Ord a) => [a] -> a
minn [] = error "min of an empty list attemped"
minn [x] = x
minn (x : xs)
  | x <= minTail = x
  | otherwise = minTail
  where
    minTail = minn xs

quicksort' :: (Ord a) => [a] -> [a]
quicksort' [] = []
quicksort' (x : xs) =
  let smaller = quicksort' [a | a <- xs, a <= x]
      bigger = quicksort' [a | a <- xs, a > x]
   in smaller ++ [x] ++ bigger

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x : xs) =
  let smallerSorted = quicksort [a | a <- xs, a <= x]
      biggerSorted = quicksort [a | a <- xs, a > x]
   in smallerSorted ++ [x] ++ biggerSorted
