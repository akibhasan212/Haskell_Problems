--Name: Akib Hasan Aryan
--UCID:30141456
import Prelude
data SF a = SS a | FF deriving (Eq, Show)
--sf divide function takes two integers and returns their integer division as SS int if possible, otherwise
shows failure with FF.
--Integer division not possible only in case where denominator is 0 and so if b=0 then it gives FF
--Integer division is possible in all other cases of integers and so gives us the appropriate value
sf_divide :: Integer -> Integer -> (SF Integer)
sf_divide a b
 | b==0 = FF
 | otherwise = SS(div a b)

--sf_remainde gives us remainder in cases where it exists
sf_remainder a b
 | (mod a b == 0) = FF
 |otherwise = SS(mod a b)
--matches takes an integer value c and an integer list and returns a list containing a list with the same
number of integers c as it occured in the input list
--If length of input list is 0, then it just returns an empty list
--else, compare head of input list with the integer c, if it is a match take a list of length one that only
contains c then add this to another recursive call of matches with input list in that being tail of initial
input list
--If not a match, just call match again with the tail.
--In this way, anytime there is a match, lists of length one containing c will be added to eachother and
this will only happen the number of times c appears in input list
--Thus, we will get list that contains c the number of times it occurs in initials list
matches :: Integer -> [Integer] -> [Integer]
matches a b
 | (length b) == 0 = []
 |((head b) == a) = [a] ++ (matches a (tail b))
 |otherwise = matches a (tail b)
--sf_matches carries out the above algorithm but just that in the case of no match happening, instead of
an empty list FF is returned
--Else SS with the list containing the integer the number of times it occurs in the input list
sf_matches :: Integer -> [Integer] -> (SF [Integer])
sf_matches a b
 | (length (matches a b)) == 0 = FF
 |otherwise = SS (matches a b)
--Takes two integers and gives true if they are coprime and false otherwise
--For the secondh guard, if a/b gives us no remainder it means b is a factor of a and so there is no way
their common divisor is only 1, in cases where b != 1
coprime :: Integer -> Integer -> Bool
coprime a b
 | a<0 || b<0 =False
 | ((mod a b) == 0) && (b==1) = True
 | (mod a b) == 0 = False
 | otherwise = coprime b (mod a b)
totient_helper :: Integer -> Integer -> Integer
totient_helper a b
 |(coprime a b) && a>b = 1 + ( totient_helper a (b+1))
 |a>b = totient_helper a (b+1)
 |otherwise = 0
sf_totient :: Integer -> (SF Integer)
sf_totient a
 |a<=0 = FF
 |otherwise = SS(totient_helper a 1)

--Helper function to find stirling number of the second kind which returns number of ways n objects can
be split into k subsets
--If k=1, then there is only one subset required so the answer is 1
--Else, if n=k, then that means there is only one unique combination of non-empty subsets as any other
combination of splitting n objects would produce empty subsets
--Otherwise, we call the recursive algorithm to find stirling number of second kind to find the stirlingn
umber
--Helper function is then used in main function to give us SS value of sitrling number of FF in case where
either n or k is 0
stirling_help :: Integer -> Integer -> Integer
stirling_help a b
 | b ==1 = 1
 | a==b = 1
 | otherwise = (b*(stirling_help (a-1) b)) + (stirling_help (a-1) (b-1))

stirling_number_2nd :: Integer -> Integer -> (SF Integer)
stirling_number_2nd a b
 | a==1 && b==1 = SS(1)
 | a==0 || b==0 = FF
 | otherwise = SS(stirling_help a b)
bell_helper :: Integer -> Integer -> Integer
bell_helper a b
 |a==b = 1
 |otherwise = (stirling_help a b) + (bell_helper a (b+1))
nth_bell_number :: Integer -> (SF Integer)
nth_bell_number a
 | a>=0 = SS (bell_helper a 1)
 | otherwise = FF


--Problem 1 part a:
--Takes two lists and returns values of list alternated with eachother starting with second list
--If either list is empty then returns just the other list without alternating since there's nothing to 
alternate
--If both lists have elements, then first makes a list with head of second and first list respectively then 
concatenates it with calling the function again with the tails of both 
--This way, each time the second list element is added in front first list element until one list is empty.
riffle :: [a] -> [a] -> [a] 
riffle ls ln
 |(length ln) == 0 = ls
 |(length ls) == 0 = ln
 |otherwise = [(head ln), (head ls)] ++ riffle (tail ls) (tail ln)
--Problem 1 part b
--Takes a list and returns a list containing all possible subsets of that list
--If list is empty then there is only one subset
--Otherwise, Adds head to each element by mapping ls: to call of powerset with tail which in turn 
produces smaller parts of the tail to which head can be added to to produce subsets
--PowerSet also called on just tail so that each element goes through process given above minus the 
original head
powerSet :: [a] -> [[a]]
powerSet [] = [[]]
powerSet (ls:lss) = powerSet lss ++ map (ls:) (powerSet lss)
--Problem 2 part a 
data Nat = Z | S Nat deriving(Show)
--Takes two Nat and sees if they are same
--If both are Z then that means they are same, otherwise, if one is Z and the other is not then it means 
they are different
--Otherwise, If both have S, then first remove S from both and then call natEq on remainder of both. If 
they do not have same number of S one of them will become Z before the other which will invoke base 
cases
natEq :: Nat -> Nat -> Bool
natEq Z Z = True
natEq _ Z = False
natEq Z _ = False
natEq (S a) (S b) = natEq a b
--Problem 2 part b
--Adds two Nat values together
--If Both are Z then nothing to add so return zero, Also, if one is zero while the other is not, then just 
return the non-zero value since 0 makes no different
--Else, if both have S, then add both intial S and then call function with remainder.
addNat :: Nat -> Nat -> Nat
addNat Z Z = Z
addNat Z a = a
addNat a Z = a
addNat (S a) (S b) = S(S(addNat a b))
--Problem 2 part c
--Sees if a Nat is even. If it is Z then it is true, if it is S Z then it means 1 so false, otherwise, if it contains 
more than one S, remove first two S and call function again on remainder
--This way we will get Z if it is even number of S or S Z if it is odd
isNatEven :: Nat -> Bool
isNatEven Z = True
isNatEven (S Z) = False
isNatEven (S(S b)) = isNatEven b
--Problem 2 part d
--Converts a nat data type to int
--If it finds Z then stops recursion otherwise keeps adding 1 for each S
natToInt :: Nat -> Int
natToInt Z = 0
natToInt (S a) = 1 + (natToInt a)
--Converts ints to Nats
--If it goes to Zero then just gives Z otherwise keeps adding an S each time
intToNat :: Int -> Nat
intToNat 0 = Z
intToNat b = S(intToNat (b-1))
--Problem 3 part a
--Adds all negative integers in a list together
--Uses foldr1 to add (+) function to full list which has been filtered using a lambda function to leave only 
the negative numbers
negativeSum :: [Integer] -> Integer
negativeSum a = foldr (+) (0) (filter (\x -> x<=0) (a))
--Problem 3 part b 
--Returns max value in a list using helpers. First make a function maxer which basically compares two 
numbers to return maximum
--Applies this to the main function list using folder to get max value
maxer :: Integer -> Integer -> Integer
maxer a b 
 | a<b = b
 | otherwise = a
myMaximum :: [Integer] -> Integer
myMaximum a = foldr1 (maxer) (a)
--Problem 4
class Shape a where
 area :: a -> Double
 perimeter :: a -> Double
data Circle = Circle Double
data Rectangle = Rectangle Double Double
instance Shape Circle where
 area (Circle r) = pi*(r**2)
 perimeter (Circle r) = (2*pi*r)
instance Shape Rectangle where
 area (Rectangle w h) = (w*h)
 perimeter (Rectangle w h) = (2*(w+h))

--Problem 5
data GTree a = Leaf a | Gnode [GTree a] 
--numberOfLeaves uses helper function leaf_help to find number of leaves in a Gtree
--numberOFLeaves returns one for one leaf, if a Gnode x, processes head of x with recursive call and 
gives tail to helper since NumberofLeaves cannot take list input
--Helper gives head of list to numberOfLeaves and calls itself with tail again and returns 0 on empty list 
leaf_help :: [GTree a] -> Int
leaf_help [] = 0
leaf_help x = numberOfLeaves(head x) + leaf_help(tail x)
numberOfLeaves :: (GTree a) -> Int
numberOfLeaves (Leaf a) = 1
numberOfLeaves (Gnode x) = (numberOfLeaves (head x)) + (leaf_help (tail x))
--FInds depth of Gtree with helper depth1. Depth gives 0 for a leaf, but if it find Gnode x then adds 1 and 
then processes head of x and calls helper on tail of x
--depth 1 calls depth on head of tail and calls itself on tail of tail
--Depth will basically be the number of Gnodes found
depth1 :: [GTree a] -> Int
depth1 [] = 0;
depth1 x = (depth (head x)) + (depth1 (tail x))
depth :: (GTree a) -> Int
depth (Leaf a) = 0
depth (Gnode x) = 1 + (depth1 x)
--findVal finds value in Gtree if present. if it finds a leaf with value then returns true, otherwise gives 
false
--Processes head of Gnode and ors it with helper taking tail. This way, even one true will result in a true 
result since true will happen only once amongst many falses
--Helper calls head of list with findVal and processes tail and returns false on empty lists.
find_helper :: (Ord a) => a -> [GTree a] -> Bool
find_helper _ [] = False
find_helper a x = (findVal a (head x)) || (find_helper a (tail x))
findVal ::(Ord a) => a -> (GTree a) -> Bool
findVal a (Leaf b) 
 |a==b = True
 |otherwise = False
findVal a (Gnode x) = (findVal a (head x)) || (find_helper a (tail x))



