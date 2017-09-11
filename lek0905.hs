import Test.QuickCheck

{-
--Lists


--myLottoNumbers :: [Int] -- a list of Int, fixed space
--myLottoNumber = [19,24,66,91,97]

--winningLottos :: [[Int]] -- a list of lists of Int
--winningLottos = [myLottoNumbers, [18,22,66,91,99],[20,30,40]]

validNumbers :: [Int]
validNumbers = [1..99] -- a built-in function to generate a list

fac :: Integer -> Integer
fac n = product [1..n] -- factorial function


--Example standard functions
-- (xx), reverse, length, su, product, maximum,
--Lots more in module Data.Lists

--___________________________________

power2table :: Integer -> [Integer]
-- power2table n gives a list of powers of 2 from 0 up to n
-- power2table 7 == [1,2,4,8,16,32,64,128]

power2table n = -- undefiner
 [2^x | x <- [0..n]] -- Bygger en lista genom att ta  från listan [0..n].	
sumsquares n = sum [x*x | x <- [1..n]]

-- Multiple Generators
example2generators = [x+y | y <- [100,200], x <- [1,2,3]]

powersUpto :: Integer -> [Integer]
-- powersUpto n gives all powers of 2 no larger than n
powersUpto n = [p | p <- power2table n, p <= n] -}
-----------------------------------------------------------------------------------------
--Define own datatypes--
--Example of datatype definition:
-- data Bool = True | False




-- The suits (An example of enumeration type)
data Suit = Hearts | Diamonds | Clubs | Spades-- declaring a new datatype "Suit"
 deriving (Eq,Show)
-- Concecpt [c] Show

--Colour 
data Colour = Red | Black
 deriving (Eq,Show)
-- colour (Eq)

{-colour :: Suit -> Colour
colour s | s == Hearts = Red | s == Diamonds = Red | otherwise = Black
-}
colour Hearts = Red
colour Diamonds = Red
colour _ = Black --  "-" Don't care pattern

data Rank = Numeric Int | Jack | Queen | King | Ace
 deriving (Eq,Show)	
-- Datatype invariant
prop_Rank :: Rank -> Bool
prop_Rank (Numeric n) = n > 1 && n < 11
prop_Rank _ = True 

--rank1 'rankBeats' rank2 ?
--when is one rank higher than another?
-- "longhand" defintion in the lecture notes.
rankBeats :: Rank -> Rank -> Bool
rankBeats _ Ace = False
rankBeats Ace _ = True
rankBeats _ King = False
rankBeats (Numeric m) (Numeric n) = m > n   -- Att kolla efter flera olika saker kallas för pattern matching

--prop_rankBeats r1 r2 = r1 `rankBeats` r2 || r2 `rankBeats` r1

data Card = Card Rank Suit
 deriving (Eq,Show)

oneEye :: Card
oneEye = Card King Diamonds --  Rank suit with label Card

-- Projection functions
rank :: Card -> Rank
rank (Card r _) = r -- r = rank, s = suit pattern matching, A function which given a card defines it rnk. _ används eftersom att vi inte bryr om suit i det här fallet

suit :: Card -> Suit
suit (Card _ s) = s 
--Kan även skrivas som data Card = Card [rank::Rank, suit::Suit]
--data Person = Person [String] String Int Int Int Int
--Kan skrivas på ett bättre sätt
type Name = String
type Year = Int
type Month = Int
type Day = Int
type Pnr = Int

{-
data Person = Person [Name] Name Year Month Day Pnr

year :: Person -> Year
year (Person _ _ y _ _ _) = y   
-\
-- ...
-- type synonyms 
-- type String = [Char]
-}
data Person = Person { firstnames :: [Name]
                     , familyname :: Name
					 , year :: Year
					 , month :: Month
					 , day :: Day
					 , pnr :: Pnr
					 }

turing :: Person
turing = Person ["Alan", "Mathison"] "Turing" 1912 6 23 0 

---------------------------------------------------------------------
--cardbeats card1 card2 checks if card1 beats card2
--w & w/o pattern matching
cardBeats :: Card -> Card -> Bool
cardBeats (Card r1 s1) (Card r2 s2) = s1 == s2 && r1 `rankBeats` r2 -- använd pattern matching så ofta det går

--Alterativ lösning
--cardBeats c1 c2 = suit c1 == suit c2 && rank c1 `rankBeats` rank c2
------------------------------------------------------------------

type Hand = [Card]

--allClubs :: Hand -> Hand
--allClubs cs = [ c | c <- cs, suit c == Clubs]

--allSpades :: Hand -> Hand <- Funkar men är inte en bra lösning. 
--allSpades cs = [ c | c <- cs, suit c == Spades]

--select
--select all the cards from the hand of the given suit

select :: Suit -> Hand -> Hand
select s cs =  [ c | c <- cs, suit c == s] -- <- Bättre lösning 

allClubs :: Hand -> Hand
allClubs cs = select Clubs cs

-------------------------------------------------
--(:) and [] are the constructors:
-- the basic building blocks for lists

-- Prelude functions null, head and tail
null' [] = True
null' _ = False

--tail' :: [t] -> t
--tail' (_:xs) = xs

head' :: [t] -> t 
head' (x:_) = x


-- polymorphic types

---------------------------------------------------------------------------

--Recall recursion
fac 0 = 1
fac n | n > 0 = n * fac (n-1)

--size
size []     = 0
size (x:xs) = 1 + size xs    
				