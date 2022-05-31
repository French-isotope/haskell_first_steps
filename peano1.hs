main :: IO ()
main = return ()



-- toto = 10
-- data JoueurType = Joueur Int String
--  deriving (Show)
  
-- me = Joueur 9001 "Gautier"
-- you = Joueur 100001 "Guillaume"


-- liste1 = (1 : (2 : (3 : (4 : (5 : (6 : (7 : (8 : (9 : [])))))))))


data Peano = Zero | Successor Peano
 deriving (Show, Eq)
 

zero = Zero
one = Successor Zero
two = Successor ( Successor Zero )
three = Successor two
four = Successor three
five = Successor four
six = Successor five
seven = Successor six
eight = Successor seven
nine = Successor eight
ten = Successor nine

addPP :: Peano -> Peano -> Peano
addPP a b = case a of
 Zero -> b
 Successor n -> addPP n (Successor b)

peanToInt :: Peano -> Int
peanToInt a = case a of 
 Zero -> 0
 Successor n -> 1 + peanToInt n

mulPP :: Peano -> Peano -> Peano
mulPP a b = case a of 
 Zero -> zero
 Successor n -> addPP ( mulPP n b ) b



-- len :: List
-- exo 4 implémenter eq ou ==
-- equal :: Peano -> Peano -> Bool

 
 
 
 
 
 
