module A3c where

import Data.Char
import Data.Maybe
--import Control.Monad
import Data.List

--onlyLowercase returns a list of strings only starting with lowercase letters
--parameters [String] ([[Char]] == [String])
--returns [String]
onlyLowercase::[[Char]] -> [[Char]]
onlyLowercase [] = []
onlyLowercase (x:xs) =
  filter (\(c:cs) -> isLower c) (x:xs)

--longestString folds through a list, constantly comparing the elements by length
--and returns the longest string in the list
--parameters: [String]
--returns String
longestString::[String] -> String
longestString [] = ""
longestString (x:xs) =
  foldl (\a b -> if((length b) > (length a)) then b else a) "" (x:xs)


--longestSTring' behaves the same way as longestString to fold through the list,
--however when two strings are the same length the accumulator is updated, so the
--returned value is the longest/equal string found latest in the list
--parameters [String]
--return String
longestString'::[String] -> String
longestString' [] = ""
longestString' (x:xs) =
  foldl (\a b -> if((length b) >= (length a)) then b else a) "" (x:xs)

--longestString3 behaves the exact same way as longestString, however it uses the helper function
--paramters [String] -> String
--return type: String
longestString3::[String] -> String
longestString3 = longestStringHelper (>)

--longestString4 behaves the same way as longestString', however it uses the helper function to do so
--paramters [String]
--return type String
longestString4::[String] -> String
longestString4 = longestStringHelper (>=)

--longestStringHelper takes a function that compares two ints, as well as a list of strings
--the function is used to compare the length of two strings from the list using foldl. Returns the longest string.
--recency of that string is determined by the function, (>) is the least recent, (>=) is the most recent
--parameters (Int -> Int -> Bool) [String]
--return String
longestStringHelper:: (Int -> Int -> Bool) -> [String] -> String
longestStringHelper _ [] = []
longestStringHelper f (x:xs) =
  foldl (\a b -> if(f (length b) (length a)) then b else a) "" (x:xs)

--combines LongestString3 and onlyLowercase using the ( . ) operator. onlyLowercase returns a list of
--strings with only lowercase starting letters, longesString3 then returns the longest of those strings
--parameters [String]
--return type String
longestLowerCase:: [String] -> String
longestLowerCase [] = ""
longestLowerCase (x:xs) =
  (longestString3 . onlyLowercase) (x:xs)

--revStringRev returns the all-lowercase version of a given string, but backwards. Using the ( . ) operator
--the revStringHelper retuns the original string but in all lowercase, the reverse function then reverses that string
--parameters String
--return type String
revStringRev:: String -> String
revStringRev "" = ""
revStringRev (x:xs) =
  (reverse . revStringHelper) (x:xs)

--revStringHelper takes a list of chars (string) and returns a list of chars, containing the same chars
--but all lowercase
--paramters String
--return type String
revStringHelper::[Char] -> [Char]
revStringHelper [] = []
revStringHelper (x:xs)
  | isUpper x = (toLower x) : revStringHelper xs
  |otherwise = x : revStringHelper xs

--firstAnswer is given a function and a list of something. The function is applied to every element of the list,
--if the function returns nothing then the firstAnswer recursively calls itself on the rest of the list, if the function
--returns Just something, then that value is returned
--parameters (a->Maybe b), [a]
--return type Maybe b (Just b or Nothing)
firstAnswer:: (a->Maybe b) -> [a] -> Maybe b
firstAnswer _ [] = Nothing
firstAnswer p (x:xs)
  |(isJust (p x)) = (p x)
  |(isNothing (p x)) = firstAnswer p xs
  |otherwise = firstAnswer p xs

--allAnswers is similar to firstAnswer, it applies the function to every element of the list, if any results of the function f
--is Nothing, the entire answer is nothing, otherwise the allAnswers returns the list of all answers
--parameters (a->Maybe [b]), [a]
--return type Maybe [b]
allAnswers::(a->Maybe [b]) -> [a] -> Maybe [b]
allAnswers _ [] = Nothing
allAnswers p (x:xs) = allAnswersHelper p (Just []) (x:xs)

--only called if the original list exists, the helper goes through the list, adding new values (using monad append) to our accumulator
--if the given function ever returns nothing, then allAnswersHelper returns Nothing. If it gets to an empty list, allAnswersHelper returns
--the current accumulator list
--parameters (a->Maybe[b]), Maybe [b], [a]
--return type Maybe [b]
allAnswersHelper::(a->Maybe[b]) -> Maybe[b] -> [a] -> Maybe[b]
allAnswersHelper a acc [] = acc
allAnswersHelper a acc (x:xs)
  |(isNothing (a x)) = Nothing
  |otherwise = allAnswersHelper a (mappend  acc (a x)) xs

data Pattern = WildcardPat | VariablePat (String) | UnitPat | ConstantPat (Int) | ConstructorPat (String, Pattern) | TuplePat ([Pattern]) deriving (Eq, Show)
data Value = Constant (Int) | Unit | Constructor (String, Value) | Tuple [Value] deriving (Eq, Show)

--applies f1 if given WildCardPats which takes an empty tuple and returns an a
--applies f2 to x if given variable pat which takes a string and returns an a
--If given a constructor pattern it recalls g with the pattern in the constructor
--if given a tuple pattern it returns the foldl of g with all the patterns in
--the tuple, ads the accumulator to it
--anything else it returns 0

--g takes a pattern and returns a value
--only uses specific functions based on what pattern it is given
g f1 f2 pat =
 let
   r = g f1 f2
 in
   case pat of
     WildcardPat -> f1 ()
     VariablePat x -> f2 x
     ConstructorPat (_, p) -> r p
     TuplePat values -> foldl (\i p -> (r p) + i) 0 values
     _ -> 0


--countWildcards passes in two anonymous functions to g, if g is given a wildcard function it returns 1, else it returns 0
--this returns the number of wildcard patters because if given a TuplePat g will fold through the list of patters and return
--the value of f1 added together at every step
--paramters Pattern
--return type Int
countWildcards:: Pattern -> Int
countWildcards p = g (\ x -> 1) (\x -> 0) p

--countWildAndVariableLengths, returns either the length of a variablePat's name, or 1 if it is given a wildcard. This
--works because if given a TuplePat the fold will accumulate all of these values together
--parameters Pattern
--return type Int
countWildAndVariableLengths:: Pattern -> Int
countWildAndVariableLengths p = g (\ x -> 1) (length) p

--countAVar returns the number of times a specific string occurs in a pattern, using g and having the foldl accumulate the
--values, I pass in an anonymous function that returns 1 if the string appears in a pattern, and 0 if not.
--parameters (String, Pattern)
--return type Int
countAVar:: (String, Pattern) -> Int
countAVar (s, p) = g (\x -> 0) (\x -> if(s == x) then 1 else 0) p


--checkPat checks a pattern to see if there is a repeat in any of the Strings used in the pattern. This is
--achieved using two helper functions, one that constructs a list of all the strings in a pattern, and another
--that returns true or false if that list contains any repeating values.
--parameters Pattern
--return type Bool
checkPat:: Pattern -> Bool
checkPat p = (checkRepeats . checkPatHelperStringList) p

--checkPatHelperStringList is similar to g. The case handles the 4 different possibilities that we care about
--to find Strings in patterns. The foldl will accumulate the STring lists into one list if a TupleConstructor is passed,
--otherwise the function will return a list containing the given pattern's string or an empty list
--parameters Pattern
--return type Bool
checkPatHelperStringList:: Pattern -> [String]
checkPatHelperStringList p =
    case p of
      VariablePat x -> [x]
      ConstructorPat (x, _) -> [x]
      TuplePat val -> foldl (\x y -> (checkPatHelperStringList y) ++ x) [] val
      _ -> []

--checkRepeats iterates through a whole list to see if any given element is the same as any other element
--it performs that operation for every element in the list and returns false if any elements are the same
--parameters [String]
--return type Bool
checkRepeats:: [String] -> Bool
checkRepeats [] = True
checkRepeats (x:xs)
  |(x `elem` xs) = False
  |otherwise = checkRepeats xs

--match takes a value, pattern tuple and gos through a variety of cases to determine if the value and pattern match.
--if they do, the appropriate binding is returned. If a tuplePat is given, and matches with its given value, allAnswers is
--called with match which also returns the list of bindings
--parameters (Value, Pattern)
--return type Maybe [(String, Value)]
match:: (Value, Pattern) -> Maybe [(String, Value)]
match (v, p) =
  case (v, p) of
    (_ ,WildcardPat) -> Just []
    (_, VariablePat s) -> Just [(s, v)]
    (Unit, UnitPat) -> Just []
    (Constructor(s1, x), ConstructorPat(s2, y)) -> if s1 == s2 then match (x, y) else Nothing
    (Constant z1, ConstantPat z2) -> if z1 == z2 then Just [] else Nothing
    (Tuple vs, TuplePat ps) -> if((length vs) == (length ps)) then (allAnswers match (zip vs ps)) else Nothing
    (_, _) -> Nothing

--firstMatch uses match to return the first matching pattern to a given value. passing match into firstAnswer will return this
--parameters Value [Pattern]
--return type Maybe[(String, Value)]
firstMatch:: Value -> [Pattern] -> Maybe [(String, Value)]
firstMatch v ps = (firstAnswer match [(v, j)| j <- ps])
