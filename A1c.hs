module A1c where
--sDotProduct takes in two tuples and a scaling integer
--it finds the dot product of the two tuples and then multiplies it
--by the scalar and returns a Num
--sDotProduct Num tuple tuple --> Num
sDotProduct:: Num a=> a -> (a, a) -> (a, a) -> a
sDotProduct scale p1 p2 =
  scale * (((fst p1) * (fst p2)) + ((snd p1) * (snd p2)))


--distance function takes two tuples and finds the distance between the
--two using distance formula. It returns a Num

distance:: Floating a => (a, a) -> (a, a) -> a
distance p1 p2 =
  (sqrt ((((fst p2) - (fst p1))^2) + (((snd p2) - (snd p1))^2)) )


--tripleDistance takes in two 3-tuples and finds cartesian distance between them
--since there is no easy function for getting items from a triple, I used
--pattern recognition to return specific elements in a triple, then used those
--values in the same equation as distance, but with an extra dimension
fst3:: (a, b, c) -> a
fst3 (x, y, z) = x

snd3::(a, b, c) -> b
snd3 (x, y, z) = y

thr3::(a, b, c) -> c
thr3 (x, y, z) = z

tripleDistance:: Floating p => (p, p, p) -> (p, p, p) -> p
tripleDistance x1 x2 =
  (sqrt ((((fst3 x2) - (fst3 x1))^2) + (((snd3 x2) - (snd3 x1))^2) +
  (((thr3 x2) - (thr3 x1))^2 )) )


--recursively iterates through the list and tries to find the smallest # in the
--list
--uses pattern recognition as a base case. If statement compares the head of the
--original list to the smallest number of the rest of the list(done recursively)
--and returns whichever is smaller

findMin :: (Ord a) => [a] -> a

findMin [] = error "Empty list"
findMin [x] = x
findMin l =
  let
     x = head l
     y = findMin(tail l)
    in if x < y
      then x
      else y

--tupleDotProduct takes two lists of numbers, whatever size, and computes
--the dot product. The problem is solved recursively, adding the multiplication
--of the current head of the list, to the recursive call of the tail of the list
--pattern recognition is used for the base case, when two empty lists are given
--the function returns 0. Could also be done with an if statement and checking
--the length of the lists
tupleDotProduct:: Num p=> [p] -> [p] -> p
tupleDotProduct [][] = 0
tupleDotProduct l1 l2 = (head l1 * head l2) + tupleDotProduct (tail l1) (tail l2)


--revZip2Lists takes two lists, iterates through them and appends an item from
--each list as pairs into one list. The recursion works by appending the current
--pair(as a list of a pair) to the end of the rest of the recursive list. This
--made the pairs appear in reverse order.

revZip2Lists:: [a] -> [b] -> [(b, a)]
revZip2Lists l1 l2 = if ((length l1) > 0) && ((length l2) > 0)
  then (revZip2Lists (tail l1) (tail l2)) ++ [(head l2, head l1)]
  else []


--everyThird takes a list of anything and returns a list containing only every
--element in the list. This was solved recursively, if the list is smaller than
--three elements return an empty list. If its not then take the third element of
--the list (head (tail (tail list))) (this was so that the very first element
--of the list wasn't included) and then call the function on the rest of the list
--after that element, (tail (tail (tail list)))
everyThird:: [a] -> [a]
everyThird xs =
  if (length xs) < 3
    then []
    else (head (tail (tail xs))) : everyThird (tail (tail (tail xs)))
