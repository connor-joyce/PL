module A2c where

removeAllExcept :: (Eq a) => a -> [a] -> [a]
removeAllExcept _ [] = []
removeAllExcept n lst
  | (n == (head lst)) = ([(head lst)] ++ (removeAllExcept n (tail lst)))
  | otherwise = (removeAllExcept n (tail lst))

removeAll :: (Eq a) => a -> [a] -> [a]
removeAll _ [] = []
removeAll n lst
  | (n /= (head lst)) = ([(head lst)] ++ (removeAll n (tail lst)))
  | otherwise = (removeAll n (tail lst))

substitute :: (Eq a) => a -> a -> [a] -> [a]
substitute _ _ [] = []
substitute x y lst
  |(x == (head lst)) = ([y] ++ (substitute x y (tail lst)))
  |otherwise = ([(head lst)] ++ (substitute x y (tail lst)))

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

nodeValue::TriTree a -> a
nodeValue EmptyNode = error "Empty Tree"
nodeValue (TriNode a la ma ra) = a

leftChild::TriTree a -> TriTree a
leftChild EmptyNode = error "Empty Tree"
leftChild (TriNode a la ma ra) = la

middleChild::TriTree a -> TriTree a
middleChild EmptyNode = error "Empty Tree"
middleChild (TriNode a la ma ra) = ma

rightChild::TriTree a -> TriTree a
rightChild EmptyNode = error "Empty Tree"
rightChild (TriNode a la ma ra) = ra

inTree :: Eq a => a -> TriTree a -> Bool
inTree _ EmptyNode = False
inTree n (TriNode v l m r)
  |(n == v) = True
  |otherwise = ((inTree n l) || (inTree n m) || (inTree n r))

leafList :: TriTree a -> [a]
leafList EmptyNode = [];
leafList (TriNode v EmptyNode EmptyNode EmptyNode) = [v]
leafList (TriNode v l m r) = ((leafList l) ++ (leafList m) ++ (leafList r))

inOrderMap :: (a->b) -> TriTree a -> TriTree b
inOrderMap _ EmptyNode = EmptyNode
inOrderMap f (TriNode v l r m) = (TriNode (f v) (inOrderMap f l) (inOrderMap f m) (inOrderMap f r))

preOrderFold::(b->a->b) -> b -> TriTree a -> b
preOrderFold _ acc EmptyNode = acc
preOrderFold f acc (TriNode v l m r) =
  --(f (f (f (f acc v) (preOrderFold f acc l)) (preOrderFold f acc m)) (preOrderFold f acc r))
  --this only works all declared types are b
  (f acc v)
