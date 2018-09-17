module A1c where


--sDotProduct: calculates the dot product between two tuples
--Parameters: (Num, Num) (Num, Num)
--Result: Num
sDotProduct:: Num a=> a -> (a, a) -> (a, a) -> a
sDotProduct scale p1 p2 =
  scale * (((fst p1) * (fst p2)) + ((snd p1) * (snd p2)))


--distane: uses the distance formula to find the distance between to 2D points
--Parameters: takes two tuples of type floating
--Result: returns a floating integer because sqrt is required in the
--distance formula
distance:: Floating a => (a, a) -> (a, a) -> a
distance p1 p2 =
  (sqrt ((((fst p2) - (fst p1))^2) + (((snd p2) - (snd p1))^2)) )


--fst3, snd3, thr3 all take in triples and return the first, second, and third
--values respectively.
--Paremeters: triples of a
--Returns: a
fst3:: (a, b, c) -> a
fst3 (x, y, z) = x

snd3::(a, b, c) -> b
snd3 (x, y, z) = y

thr3::(a, b, c) -> c
thr3 (x, y, z) = z

--tripleDistance finds the distance between two 3d points using the distance
--formula
--Parameters: tripleDistance takes in two triples of Nums (floating)
--Result: returns a floating point integer
tripleDistance:: Floating p => (p, p, p) -> (p, p, p) -> p
tripleDistance x1 x2 =
  (sqrt ((((fst3 x2) - (fst3 x1))^2) + (((snd3 x2) - (snd3 x1))^2) +
  (((thr3 x2) - (thr3 x1))^2 )) )


--findMin: recursivelry iterates through a list and returns the smallest
--value in the lists
--Parameters: list of numbers
--Returns: nubmer
findMin[] = error "No minimum exists in an empty list"
findMin [x] = x
findMin l =
  let
     x = head l
     y = findMin(tail l)
    in if x < y
      then x
      else y

--tupleDotProduct: returns the dot product of two points of any dimension
--Parameters: [Num] [Num]
--Result: Num
tupleDotProduct:: Num p => [p] -> [p] -> p
tupleDotProduct [][] = 0
tupleDotProduct l1 l2 =  (head l1 * head l2) + tupleDotProduct (tail l1) (tail l2)


--revZip2Lists: recursively iterates through two lists, and creates a list
--of tuples of each element of both lists, in reverse order
--Parameters [a] [b]
--Results: [(bx, ax), (bx-1, ax-1), ... (b0, a0)]
revZip2Lists:: [a] -> [b] -> [(b, a)]
revZip2Lists l1 l2 = if ((length l1) > 0) && ((length l2) > 0)
  then (revZip2Lists (tail l1) (tail l2)) ++ [(head l2, head l1)]
  else []



--everyThird: Takes a list of anything and returns a list containing every third
--element of the original list
--Parameters: [a]
--Results : [a3, a6, a9, a12, ...]
everyThird:: [a] -> [a]
everyThird xs =
  if (length xs) < 3
    then []
    else (head (tail (tail xs))) : everyThird (tail (tail (tail xs)))
