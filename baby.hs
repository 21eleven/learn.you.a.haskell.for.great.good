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
