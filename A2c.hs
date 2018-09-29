module A2c where

--removeAllExcept returns a list of all elements in a list that are equal to a given
--value
--parameters: (Eq a) a and [a]
--results: [a]
removeAllExcept :: (Eq a) => a -> [a] -> [a]
removeAllExcept _ [] = []
removeAllExcept n lst
  | (n == (head lst)) = ([(head lst)] ++ (removeAllExcept n (tail lst)))
  | otherwise = (removeAllExcept n (tail lst))

--removeAll creates a list of elements of a given list that are not equal to a given value
--parameters: (Eq a) a [a]
--results: [a]
removeAll :: (Eq a) => a -> [a] -> [a]
removeAll _ [] = []
removeAll n lst
  | (n /= (head lst)) = ([(head lst)] ++ (removeAll n (tail lst)))
  | otherwise = (removeAll n (tail lst))

--substitute creates a list where all instances of a specific value in the given list
--are replaced with a different given value
--parameters: (Eq a) a a [a]
--results: [a]
substitute :: (Eq a) => a -> a -> [a] -> [a]
substitute _ _ [] = []
substitute x y lst
  |(x == (head lst)) = ([y] ++ (substitute x y (tail lst)))
  |otherwise = ([(head lst)] ++ (substitute x y (tail lst)))

--mergeSorted3 takes three sorted lists and returns one sorted list containing
--all of the elements of the three given lists
--parameters (Ord a) [a] [a] [a]
--results: [a]
mergeSorted3::(Ord a) => [a] -> [a] ->[a] ->[a]
mergeSorted3 [] [] [] = []

mergeSorted3 l1 [] [] = l1
mergeSorted3 [] l2 [] = l2
mergeSorted3 [] [] l3 = l3

mergeSorted3 (x:xs) (y:ys) []
  |(x <= y) = x : mergeSorted3 xs (y:ys) []
  |(y < x) = y : mergeSorted3 (x:xs) ys []


mergeSorted3 (x:xs) [] (y:ys)
  |(x <= y) = x : mergeSorted3 xs [] (y:ys)
  |(y < x) = y : mergeSorted3 (x:xs) [] ys


mergeSorted3 [] (x:xs) (y:ys)
  |(x <= y) = x : mergeSorted3 [] xs (y:ys)
  |(y < x) = y : mergeSorted3 [] (x:xs) ys



mergeSorted3 (x:xs) (y:ys) (z:zs)
  |(x <= y) && (x <= z) = x : mergeSorted3 xs (y:ys) (z:zs)
  |(z <= y) && (z <= x) = z : mergeSorted3 (x:xs) (y:ys) zs
  |(y <= x) && (y <= z) = y : mergeSorted3 (x:xs) ys (z:zs)



data TriTree a = EmptyNode | TriNode a (TriTree a) (TriTree a) (TriTree a)
  deriving Show
instance (Eq a) => Eq (TriTree a) where
  EmptyNode           == EmptyNode = True
  TriNode a la ma ra  == TriNode b lb mb rb = (a == b) &&
                                              (la == lb) &&
                                              (ma == mb) &&
                                              (ra == rb)
  _                   == _ = False

--returns the value of a given node, or an error if the node is Empty
--parameters: TriTree a
--results: a or error
nodeValue::TriTree a -> a
nodeValue EmptyNode = error "Empty Tree"
nodeValue (TriNode a la ma ra) = a

--leftChild returns the left most sub-tree in a given node
--parameters: TriTree a
--results: TriTree or error
leftChild::TriTree a -> TriTree a
leftChild EmptyNode = error "Empty Tree"
leftChild (TriNode a la ma ra) = la

--middleChild returns the middle sub tree of a given TriNode
--parameters: TriTree a
--results: TriTree a or error
middleChild::TriTree a -> TriTree a
middleChild EmptyNode = error "Empty Tree"
middleChild (TriNode a la ma ra) = ma

--rightChild returns the right subtree of a given TriTree node, or an error if the
--node is empty
--parameters: TriTree a
--results : TriTree a or error
rightChild::TriTree a -> TriTree a
rightChild EmptyNode = error "Empty Tree"
rightChild (TriNode a la ma ra) = ra

--inTree recursively searches every value in a given TriTree and returns
--true if a given value exists in the TriTree and false otherwise
--parameters: (Eq a) a TriTree a
--results: Boolean
inTree :: Eq a => a -> TriTree a -> Bool
inTree _ EmptyNode = False
inTree n (TriNode v l m r)
  |(n == v) = True
  |otherwise = ((inTree n l) || (inTree n m) || (inTree n r))

--leafList returns a list of every value stored in all of the leaves in the tree by
--recursively searching the tree for nodes with empty children
--parameters: TriTree a
--results: [a]
leafList :: TriTree a -> [a]
leafList EmptyNode = [];
leafList (TriNode v EmptyNode EmptyNode EmptyNode) = [v]
leafList (TriNode v l m r) = ((leafList l) ++ (leafList m) ++ (leafList r))

--inOrderMap applies a given function (taking one parameter) to every value in a given tree,
--and returns a tree with all of the new values.
--inOrderMap f (where f is a function that takes one value a and returns a value b) TriTree a
--results: TriTree b
inOrderMap :: (a->b) -> TriTree a -> TriTree b
inOrderMap _ EmptyNode = EmptyNode
inOrderMap f (TriNode v l m r) = (TriNode (f v) (inOrderMap f l) (inOrderMap f m) (inOrderMap f r))

--preOrderFold recursively iterates through a tree, in pre order traversal, and applies a function to every
--value in a tree, storing it in an accumulator which gets returned
--parameters: f (where f is a function that takes 2 parameters of type b and a and returns type b)
-- b and TriTree a
--results: b
preOrderFold::(b->a->b) -> b -> TriTree a -> b
preOrderFold _ acc EmptyNode = acc
preOrderFold f acc (TriNode v l m r) = (preOrderFold f (preOrderFold f (preOrderFold f (f acc v) l) m) r)
