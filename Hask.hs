import Data.List -- per delete x in list


generaCoppia
 nEsercizi matricola = (primo, secondo)
    where
    primo = matricola `mod` nEsercizi + 1
    secondo = (matricola `mod` (nEsercizi - 1) + primo) `mod`  nEsercizi + 1



--Numeri
--1
fattoriale :: (Integral a)=> a->a
fattoriale n = if(n<1)
    then 1
    else n*fattoriale(n-1)

fattoriale3 :: (Integral a)=> a->a --Non termina per numeri negativi
fattoriale3 0 = 1
fattoriale3 n = n * fattoriale3 ( n-1 )

f :: (Integral a)=> a->a
f n=product[1..n]

--2
--vincoli di esistenza non necessari, se n<k risultato=0
combinazioni :: (Integral a)=> a->a->a
combinazioni n k = (f n) `div` ( (f k) * (f (n-k) ) )

--3
calcComb :: Integer -> [[Integer]]
calcComb a = myPermutations[1..a]

myPermutations :: [Integer] -> [[Integer]]
myPermutations [] = [[]]
myPermutations xs = [x:ys | x<-xs, ys<-myPermutations(delete x xs)]



--Liste

--1 conta da 0
rimuoveDispari :: [a]->[a]
rimuoveDispari [] = []
rimuoveDispari [x] = [x]
rimuoveDispari (x:xs) = x : rimuoveDispari (tail xs)


rimuovePari :: [a]->[a]
rimuovePari [] = []
rimuovePari (x:xs) = (head xs) : rimuoveDispari (tail (tail xs))


rimuovePari2 :: [a]->[a]
rimuovePari2 [] = []
rimuovePari2 [x] = []
rimuovePari2 (x:xs) = (head xs) : rimuovePari2 (tail xs)


--2
sommaDispari :: (Num a)=> [a]->a
sommaDispari [] = 0
sommaDispari (x:xs) = sum (rimuovePari2 (x:xs))


--4
minOdd :: (Integral a)=> [a]->[a]
minOdd xs = take 2 [ x | x<-qs(xs), odd x] --quicksort in fondo


--5
coppie :: (Num a)=> [a]->[(a,a)]
coppie []     = []
coppie (x:xs) = (x, sum xs) : coppie xs


--6
invertedSum :: (Num a)=> [a]->[(a,a)]
invertedSum []  = []
invertedSum x = invertedSum2 x 0 -- passa la lista e la somma degli elementi precedenti=0

invertedSum2 :: (Num a)=> [a]->a->[(a,a)]
invertedSum2 [] _ = [] -- caso base
-- x è l'elemento in esame, somma è la somma di tutti i numeri precedenti
invertedSum2 (x:xs) somma = [(x, somma)] ++ invertedSum2 xs (somma+x)


--7
shiftToZero :: (Num a, Ord a)=> [a]->[a]
shiftToZero [] = []
shiftToZero xs = map (subtract (minimum xs)) xs




--Matrici: liste di liste, per righe
--1
matrix_dim :: (Num a)=> [[a]]->(Int, Int)
matrix_dim []       = (0, 0)
matrix_dim [[x]]    = (1, length [x])
matrix_dim all@(x:xs)   = m2 0 (length x) all

m2 :: (Num a)=> Int->Int->[[a]]->(Int, Int) -- input: (contatore_righe, dimensione_colonna, matrice)
m2 r c [] = (r, c)  -- terminazione quando la matrice è diventata una lista vuota
m2 r c (x:xs)  = if (length x == c) -- se la dim Colonna è corretta
                       then m2 (r+1) c xs -- incremento contatore riga
                       else (-1, -1)


--2 vector of sums of the colomns
colsums :: (Num a)=> [[a]]->[a]
colsums [[], _]  = [] -- tail returns [[],[],...]
colsums matrix = sum (map head matrix) : colsums (map tail matrix)


--3
colaltsums :: (Num a)=> [[a]]->[a]
colaltsums []   = []
colaltsums [x] = x  -- only a row
colaltsums [x, y] = zipWith (-) x y -- 2 rows
colaltsums (x: y: z: xs) = colaltsums ((zipWith (+) ( zipWith (-) x y ) z) : xs ) -- 3 or more rows


--4
colMinMax :: (Num a, Ord a)=> [[a]]->[(a,a)]
colMinMax [] = []
colMinMax x = zip (colMin x) (colMax x)

colMin :: (Num a, Ord a)=> [[a]]->[a]
colMin [] = []
colMin [x] = x
colMin (x: y : xs) =  colMin ((zipWith min x y)   :xs)

colMax :: (Num a, Ord a)=> [[a]]->[a]
colMax [] = []
colMax [x] = x
colMax (x: y : xs) =  colMax ((zipWith max x y)   :xs)


--13
transpose' :: (Num a, Ord a)=> [[a]]->[[a]]
transpose' ([]:_) = []
transpose' xs = (map head xs) : transpose (map tail xs)



--Alberi Binari di Ricerca
data Tree a = Null | Node a (Tree a) (Tree a)
  deriving ( Show, Read, Eq, Ord )


createTree :: a -> Tree a
createTree x = Node x Null Null

treeInsert :: (Ord a)=> a -> Tree a -> Tree a
treeInsert x Null = createTree x
treeInsert x (Node a left right)
    | x == a = Node x left right
    | x < a  = Node a (treeInsert x left) right
    | x > a  = Node a left (treeInsert x right)

makeTree :: (Ord a) => [a] -> Tree a
makeTree xs = foldr treeInsert Null xs



--1
sumTree :: (Num a)=>Tree a-> a
sumTree Null = 0
sumTree (Node x left right) = x + (sumTree left) + (sumTree right)

--2
treeSumOdd :: (Integral a)=> Tree a -> a
treeSumOdd Null = 0
treeSumOdd (Node x left right) = if odd(x)
    then x + (treeSumOdd left) + (treeSumOdd right) -- add the element
    else  (treeSumOdd left) + (treeSumOdd right) -- ignore the element

--3
sameSums :: (Num a, Eq a)=> [Tree a]->Bool
sameSums [] = False
sameSums x = sameElement (map sumTree x)

sameElement :: (Num a, Eq a)=> [a]->Bool
sameElement x = if False `elem` ( map (\y-> y==(head x)) x)  -- controlla la presenza del FALSE invece di fare AND
    then False
    else True

--4 appartenenza elemento
bstElem :: (Ord a)=> Tree a -> a-> Bool
bstElem Null x = False
bstElem (Node value left right ) x
    | x == value = True
    | x > value = bstElem right x
    | x < value = bstElem left x


--Alberi Generici a figli multipli
data GTree a = GNull | GNode a [GTree a]
  deriving ( Show, Read, Eq )

singletonG :: a -> GTree a
singletonG x = GNode x [GNull]


treeInsertG :: (Ord a)=> a -> GTree a -> GTree a
treeInsertG x GNull = singletonG x
treeInsertG x (GNode a ys ) = GNode a ( [singletonG x] ++ ys)

foldTreer :: (Eq a, Show a)=> (a->[b] ->b) -> b-> GTree a-> b
foldTreer f k (GNode a []) = k
foldTreer f k (GNode a xs) = f a (map (foldTreer f k) xs)

height :: GTree a -> Integer
height GNull = -1
height (GNode x xs) = 1 + max' (map height xs)



--Quad Trees: 1 colore o 4 sezioni di colori diversi
-- C color, QT Quad Tree
data QT a = C a| Q (QT a) (QT a) (QT a) (QT a)
  deriving ( Show, Read, Eq )

--1
buildNSimplify :: (Eq a, Show a)=>  QT a-> QT a-> QT a-> QT a-> QT a
buildNSimplify (C a) (C b) (C c) (C d)=  Q (C a) (C b) (C c) (C d)
buildNSimplify (Q a b c d) (Q e f g h) (Q i j k l) (Q m n o p) = Q (Q a b c d) (Q e f g h)  (Q i j k l)  (Q m n o p)

--2
simplify :: (Eq a, Show a)=>  QT a-> QT a
simplify (Q a b c d) = if ( a == b &&  b == c && c == d )
        then a
        else Q a b c d

--4 minimum pixel needed 1 pixel per color
howManyPixels :: (Eq a, Show a)=>  QT a -> Int
howManyPixels (C a) = 1 -- 1 color in 1 pixel
howManyPixels (Q a b c d) =  (howManyPixels a + howManyPixels b + howManyPixels c + howManyPixels d)






--altri

cilindro :: (Floating a)=> a->a->a
cilindro r h =
    let areaLaterale = 2*pi*r*h
        area = pi*r^2
    in  areaLaterale + 2*area


max' :: (Ord a)=> [a]->a
max' [] = error "no input"
max' [x] = x
max' (x:xs) = max x (max' xs)


replicate' :: (Integral a)=> a->b->[b] -- replicate n times the element x
replicate' n x
    | n <= 0    = []
    | otherwise = x : replicate' (n-1) x


take' :: (Num i, Ord i)=> i->[a]->[a] -- take n elements from the list
take' n _
    | n <= 0    = [] -- base case for recursion
take' _ []      = [] -- empty case
take' n (x:xs) = x: take' (n-1) xs

-- check the existence of a in xs
elem' :: (Eq a)=> a->[a]->Bool
elem' a [] = False -- caso lista vuota
elem' a (x:xs)
    | a == x    = True
    | otherwise = elem' a xs


qs :: (Ord a)=> [a]->[a]
qs [] = [] -- se vuoto, ritorna vuoto
qs (x:xs) =
    let minori = [a| a<-xs, a <= x]
        maggiori = [a| a<-xs, a > x]
    in qs minori  ++ [x] ++  qs maggiori -- recursion


somma :: (Num a)=> [a]->a
somma xs = foldl (\accumulator x -> accumulator+x) 0 xs


calcoloBmi :: Double->Double->String
calcoloBmi peso altezza
            | bmi <= 18.5 = "sottopeso"
            | bmi <= 25.0 = "normopeso"
            | bmi <= 30.0 = "sovrappeso"
            | otherwise   = "obeso"
            where bmi = peso/altezza^2

listaBmi :: [(Double, Double)]->[Double]
listaBmi xs = [bmi w h | (w, h)<-xs]
    where bmi peso altezza = peso/altezza^2 -- funzione inline nel where


applyTwice :: (a->a)->a->a -- input: function f with one input, and 1 parameter
applyTwice f x = f (f x)

zipWith' :: (a->b->c) -> [a]->[b]->[c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

-- takes a function with inputs: a,b e return a function that takes b,a
flip' :: (a->b->c) -> (b->a->c)
flip' f y x = f x y


largestDivisible :: (Integral a) => a->a
largestDivisible y = head (filter p [100000,99999..1]) --1 prevent the negative numbers
    where p x = x `mod` y == 0


squareNumbers = [ n^2 | n<-[1..] ]
squareNumbersLessThen10000 = map (^2) [1..100]

collatzChain :: (Integral a)=> a -> [a]
collatzChain 1 = [1] -- base case
collatzChain n
                | even n =  n : collatzChain (n `div` 2)
                | odd n  =  n : collatzChain (n*3 + 1)


complemento :: (Integral a)=> [a]->a->[a]
complemento [] _ = []
complemento xs n = map (flip subtract n) xs

product' :: (Num a)=> [a]->a
product' [] = 0
product' xs = foldl (*) 1 xs


last' :: (Num a)=> [a]->a
last' = foldr1 (\_ x->x) -- foldl1 set accumulator with the first element of the list
-- the lambda function ignore the accumulator and output the current element


permutazioni :: [a]->[[a]]
permutazioni [] = [[]]
permutazioni (x:xs) = f_perm x (permutazioni xs) -- prende il primo elemento e lo attacca a tutte le permutazioni della lista di liste

f_perm :: a->[[a]]->[[a]]
f_perm c [] = [] -- mettendo [[]] si ottiene ....
f_perm c (xs:xss) = (f2_perm c xs) ++ (f_perm c xss) -- seleziona l'elemento e lo vuole inserire in una lista

f2_perm :: a->[a]->[[a]] -- data un elemento e una lista, si creano delle liste dove l'elemento è in una posizione diversa
f2_perm x [] = [[x]]
f2_perm x (y:ys) = (x:y:ys): map (y:) (f2_perm x ys)


-- matrix made of 0s and 1s
create_matrix :: Int->[[Int]]
create_matrix 0 = []
create_matrix x = if (x>0)
                    then create_matrix_aux x x
                    else [[]]

create_matrix_aux :: Int->Int->[[Int]]
create_matrix_aux 0 _ = []
create_matrix_aux rows columns = if (rows `mod` 2 == 0)
                                    then (take columns (cycle[0, 1])) : (create_matrix_aux (rows-1) columns)
                                    else (take columns (cycle[1, 0])) : (create_matrix_aux (rows-1) columns)


log2 :: Integer -> Integer
log2 0 = 0
log2 1 = 0
log2 n = 1 + ( log2 $ toInteger (n `div` 2) )
