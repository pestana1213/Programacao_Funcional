module All where
import Data.Char


-- 1) -> V

enumFromTo1 :: Int -> Int -> [Int]
enumFromTo1 x y | x == y = [x]
                | x < y = x:enumFromTo1 (x+1) y
                | otherwise = []
                
-- 2) DUVIDA

enumFromThenToo :: Int -> Int -> Int -> [Int] 
enumFromThenToo x y z = if (x <= z) then (x:(enumFromThenToo y (y+(y-1)) z))
                                    else []
-- 3) -> V

(+++) :: [a] -> [a] -> [a]
(+++) [] l = l
(+++) (h:t) l = h:( (+++) t l)  

-- 4) -> V

(!!!) :: [a] -> Int -> a
(!!!) (h:t) 0 = h
(!!!) (h:t) x = (!!!) t (x-1)

-- 5) -> V

reverse1 :: [a] -> [a]
reverse1 [] = []
reverse1 (h:t) = reverse t ++ [h]

-- 6) -> V

take1 :: Int -> [a] -> [a]
take1 0 l = []
take1 x [] = []
take1 x (h:t) | x>0 = (h:take1(x-1) t)

-- 7) -> V

drop1 :: Int -> [a] -> [a]
drop1 0 l = l
drop1 x [] = []
drop1 x (h:t) | x>0 = drop1(x-1) t

-- 8) -> V

zip1 :: [a] -> [b] -> [(a,b)]
zip1 [] l2 = []
zip1 l1 [] = []
zip1 (x:xs) (y:ys) = (x,y):zip1 xs ys

-- 9) -> V

elem1 ::  Eq a => a -> [a] -> Bool
elem1 x [] = False
elem1 x (h:t) = if (x==h) then True
                          else (elem1 x t)

-- 10) -> V

replicate1 :: Int -> a -> [a]
replicate1 0 x = []
replicate1 n x = x:replicate1 (n-1) x

-- 11) -> V

intersperse1 :: a -> [a] -> [a]
intersperse1 x [] = []
intersperse1 x [h] = [h]
intersperse1 x (h:t) = h:x:intersperse1 x t

-- 12) DUVIDA

group1 :: Eq a => [a] -> [[a]]
group1 [] = []
group1 [x] = [[x]]
group1 (x:y:xs) = let (first:rest) = group1 (y:xs)
                  in if (x /= y) then [x]:first:rest
                               else (x:first):rest

group2 :: Eq a => [a] -> [[a]]
group2 [] = []
group2 (h:t) = (h:takeWhile (== h) t) : group2 (dropWhile (== h) t)


-- 13) -> V

concat1 :: [[a]] -> [a]
concat1 [] = []
concat1 (h:t) = h ++ concat1 t

-- 14) -> V

inits1 :: [a] -> [[a]]
inits1 [] = []
inits1 l = inits1 ( init l) ++ [l] -- Init já esta pré defenida, retoma uma lista sem o seu ultimo elemento

-- 15) -> V

tails1 :: [a] -> [[a]]
tails1 [] = []
tails1 l = l: tails1 (tail l) -- Tail já pre defenida, drop do primeiro elemento da lista

-- 16) -> V

isPrefixOff :: Eq a => [a] -> [a] -> Bool
isPrefixOff [] a = True
isPrefixOff a [] = False
isPrefixOff (x:xs) (y:ys) = x==y && isPrefixOff xs ys -- Nao precisamos de if pois o && já tem valor boleano
                           
-- 17) -> V

isSuffixOff :: Eq a => [a] -> [a] -> Bool
isSuffixOff [] a = True
isSuffixOff a [] = False
isSuffixOff l (x:xs) = l == (x:xs) || isSuffixOff l xs -- Nao precisamos de if pois o || já tem valor boleano

-- 18) -> V

isSubsequenceOff :: Eq a => [a] -> [a] -> Bool
isSubsequenceOff [] a = True
isSubsequenceOff a [] = False
isSubsequenceOff (x:xs) (y:ys) = x==y && isSubsequenceOff xs ys || isSubsequenceOff (x:xs) ys

-- 19)

elemIndices1 :: Eq a => a -> [a] -> [Int]
elemIndices1 a [] = []
elemIndices1 x (h:t) | x==h = 0:map (+1) (elemIndices1 x t)
                     | otherwise = map (+1) (elemIndices1 x t)
                     
-- 20) -> V

nub1 :: Eq a => [a] -> [a]
nub1 [] = []
nub1 (h:t) = if ( elem h t) then nub1 t
             else h:nub1 t


-- 21) -> V

delete1 :: Eq a => a -> [a] -> [a] 
delete1 x [] = []
delete1 x (h:t) = if (x==h) then t
                  else h:delete1 x t

-- 22) -> V

remove1 :: Eq a => [a] -> [a] -> [a] 
remove1 l [] = l
remove1 [] l = []
remove1 l (h:t) = remove1 (delete1 h l) t

-- 23) -> V

union1 :: Eq a => [a] -> [a]-> [a]
union1 l [] = l 
union1 l (h:t) = if ( elem h l ) then (union1 l t)
                else  union1 (l ++ [h]) t

-- 24) -> V

intersect1 :: Eq a => [a] -> [a] -> [a]
intersect1 [] l = []
intersect1 (h:t) l = if ( elem h l) then ( h:intersect1 t l)
                      else (intersect1 t l)

-- 25) -> V

insert1 :: Ord a => a -> [a] -> [a] 
insert1 x [] = [x]
insert1 x (h:t) = if ( x<=h ) then ( x:h:t )
                  else ( h:insert1 x t)

-- 26) -> 

unwords1 :: [String] -> String
unwords1 [] = ""
unwords1 (h:t) = h ++ (if t==[] then "" else " ") ++ unwords1 t

-- 27) -> V

unlines1 :: [String] -> String
unlines1 [] = ""
unlines1 (h:t) = h ++ "\n" ++ unlines1 t

-- 28) DUVIDA

pMaior1 :: Ord a => [a] -> Int
pMaior1 [a] = 0
pMaior1 (h:t) = let x = pMaior1 t
                in if ( h > (t !! x)) then 0
                else 1+x

-- 29) -> V

temRepetidos1 :: Eq a => [a] -> Bool
temRepetidos1 [] = False
temRepetidos1 (h:t) = elem h t || temRepetidos1 t

-- 30) -> V

algarismos1 :: [Char] -> [Char] 
algarismos1 [] = []
algarismos1 (h:t) = if ( elem h ['0'..'9']) then h:algarismos1 t
                    else algarismos1 t

-- 31) -> V

posImpares1 :: [a] -> [a]
posImpares1 [] = []
posImpares1 [a] = []
posImpares1 (h:s:t) = s:posImpares1 t

-- 32) -> V

posPares1 :: [a] -> [a]
posPares1 [] = []
posPares1 [a] = [a]
posPares1 (h:s:t) = h:posPares1 t

-- 33) -> V

isSorted1 :: Ord a => [a] -> Bool
isSorted1 [] = True
isSorted1 [a] = True
isSorted1 (h:s:t) = s>=h && isSorted1 (s:t)

-- 34) -> V

iSort1 :: Ord a => [a] -> [a]
iSort1 [] = []
iSort1 (h:t) = insert1 h (iSort1 t)

-- 35) -> V

menor1 :: String -> String -> Bool
menor1 l "" = False
menor1 "" l = True
menor1 (x:xs) (y:ys) = x<y || menor1 xs ys

-- 36)

elemMSet1 :: Eq a => a -> [(a,Int)] -> Bool
elemMSet1 a [] = False
elemMSet1 a ((x,n):t) = a==x || elemMSet1 a t 

-- 37)

lengthMSet1 :: [(a,Int)] -> Int 
lengthMSet1 [] = 0
lengthMSet1 ((x,n):xs) = n + lengthMSet1 xs

-- 38)

converteMSet1 :: [(a,Int)] -> [a]
converteMSet1 [] = []
converteMSet1 ((x,1):xs) = x:converteMSet1 xs
converteMSet1 ((x,n):xs) = x:converteMSet1 ((x,n-1):xs)

-- 39)

insereMSet1 :: Eq a => a -> [(a,Int)] -> [(a,Int)]
insereMSet1 x [] = [(x,1)]
insereMSet1 x ((a,n):xs) = if (x==a) then ((a,n+1):xs)
                           else (a,n):insereMSet1 x xs

-- 40)

removeMSet1 :: Eq a => a -> [(a,Int)] -> [(a,Int)]
removeMSet1 x [] = []
removeMSet1 x ((a,n):xs) = if (x==a) then xs
                           else (a,n):removeMSet1 x xs

-- 41)

constroiMSet1 :: Ord a => [a] -> [(a,Int)]
constroiMSet1 [] = []
constroiMSet1 (l:ls) = insereMSet1 l (constroiMSet1 ls)

-- 42)

--partitionEithers1 :: [Either a b] -> ([a],[b]) 


-- 43)

catMaybes1 :: [Maybe a] -> [a]
catMaybes1 [] = []
catMaybes1 ((Just a):xs) = a:catMaybes1 xs
catMaybes1 (Nothing:xs) = catMaybes1 xs

-- 44)

data Movimento = Norte | Sul | Este | Oeste deriving Show

posicao :: (Int,Int) -> [Movimento] -> (Int,Int)
posicao p [] = p
posicao (x,y) (l:ls) = posicao ( case l of Norte -> (x,y+1)
                                           Sul -> (x,y-1)
                                           Este -> (x+1,y)
                                           Oeste -> (x-1,y)) ls

-- 45)

caminho :: (Int,Int) -> (Int,Int) -> [Movimento]
caminho (xi,yi) (xf,yf)
    | xi < xf = Este:caminho (xi + 1, yi) (xf, yf)
    | xi > xf = Oeste:caminho (xi - 1, yi) (xf, yf)
    | yi < yf = Norte:caminho (xi, yi + 1) (xf, yf)
    | yi > yf = Sul:caminho (xi, yi - 1) (xf, yf)
    | otherwise = []

-- 46)
vertical :: [Movimento] -> Bool
vertical [] = True
vertical (l:ls) = case l of Este -> False
                            Oeste -> False
                            otherwise -> vertical ls

--47)
data Posicao = Pos Int Int deriving Show

maisCentral :: [Posicao] -> Posicao
maisCentral [(Pos x y)] = (Pos x y)
maisCentral ((Pos x y):(Pos a b):ps) = if (x^2 + y^2) < (a^2 + b^2) then maisCentral ((Pos x y):ps) 
                                       else maisCentral ((Pos a b):ps)

-- 48)

vizinhos ::  Posicao -> [Posicao] -> [Posicao]
vizinhos (Pos x y) ps = filter (\(Pos a b) -> (abs (a - x) + abs (b - y) == 1)) ps 

-- 49)

mesmaOrdenada :: [Posicao] -> Bool
mesmaOrdenada [(Pos x y)] = True
mesmaOrdenada ((Pos x y):(Pos x2 y2):ps) = y == y2 && mesmaOrdenada ((Pos x2 y2):ps)

-- 50)

data Semaforo = Verde | Amarelo | Vermelho deriving Show

interseccaoOK :: [Semaforo] -> Bool
interseccaoOK ss = length [s | s <- ss, case s of Vermelho -> False; otherwise -> True] < 2

--------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------

-- FIHCA 2
-- 2a)
dobros :: [Float] -> [Float]
dobros [] = []
dobros (h:t) = (2*h):(dobros t)

-- b)
numOcorre :: Char -> String -> Int
numOcorre c[] = 0
numOcorre c (h:t) = if (c==h) then (1+numOcorre t)
	                          else  numOcorre t

-- c)
positivos :: [Int] -> Bool
positivos (h:t) = if (h>0) && positivos t

-- d)
soPos :: [Int] -> [Int]
soPos []=[]
soPos (h:t) = if (h<0) then soPos t
                       else (h:soPos t)

-- e)
somaNeg :: [Int] -> Int
somaNeg [] = 0
somaNeg (h:t) = if (h<0) then (h+somaNeg t)
                         else somaNeg t

-- f)
tresUlt :: [a] -> [a]
tresUlt [] = []
tresUlt (h:t) = if ( length (h:t)<=3 ) then (h:t)
                else tresUlt t

-- g)
segundos :: [(a,b)] -> [b]
segundos ((x,y):t) = y:segundos t
--OU
segundos1 :: [(a,b)] -> [b]
segundos1 []=[]
segundos1 (h:t) = (second h):segundos1 t

-- h)
nosPrimeiros :: (Eq a) => a -> [(a,b)] -> Bool
nosPrimeiros x [] = False
nosPrimeiros x ((y,z):t) | (x==y) = True 
                         | otherwise = x:nosPrimeiros z t

-- i)
sumTriplos :: (Num a, Num b, Num c) => [(a,b,c)] -> (a,b,c)
sumTriplos [] = (0,0,0)
sumTriplos [x] = x
sumTriplos ((x1,y1,z1):(x2,y2,z2):t) = sumTriplos ((x1+x2,y1+y2,z1+z2):t)


-- 4)
type Polinomio = [Monomio]
type Monomio = (Float,Int)
-- a)
conta :: Int -> Polinomio -> Int
conta l [] = 0
conta l ((x:y):t) | (l==y) = 1+conta l t
                  | (l/=y) = conta l t

-- b)
grau :: Polinomio -> Int
grau [(c,e)] = e
grau ((c1,l1):(c2,l2):t) | l1<l2 = ((c2,l2):t)
                         | otherwise = ((c1,l2):t)
-- c)
selgrau :: Int -> Polinomio -> Polinomio
selgrau e [] = []
selgrau e ((x:y):t) | (e==y) = (x:y):selgrau e t
                    | (e=/y) = selgrau e t
-- d)
deriv :: Polinomio -> Polinomio
deriv [] = []
deriv ((c1,l1):t) | (e==0) = deriv t
                  | (e/=0) = (c*(fromIntegral e), e-1):deriv t
-- e)
calcula :: Float -> Polinomio -> Float
calcula v [] = 0
calcula v ((c,e):t) = c*(v^e) + calcula v t

-- g)
mult :: Monomio -> Polinomio -> Polinomio
mult (c,e) [] = []
mult (c,e) ((c1,e1):t) = (c*c1,e*e1):mult (c,e) t

-- h)
normaliza :: Polinomio -> Polinomio
normaliza [] = []
normaliza [x] = [x]
normaliza ((c1,e1):(c2,e2):t) | (e1==e2) = normaliza ((c1+c2,e1):t)
                              | (e1/=e2) = (c1,e1):normaliza ((c2,e2):t)
-- i)
soma :: Polinomio -> Polinomio -> Polinomio
soma p1 p2 = normaliza (p1 ++ p2)
-- j)
 produto :: Polinomio -> Polinomio -> Polinomio
 produto _ [] = []
 produto [] _ = []
 produto p1 p2 = normaliza (prod p1 p2)
     where prod [] p2 = []
           prod ((c,e):p1) p2 = mult ( ((c,e) p2) ++ prod p1 p2)
-- k)
ordenaP :: Polinomio -> Polinomio
ordenaP [] = []
ordenaP ((c,e):t) = insere (c,e) (ordenaP t)
     where insere :: Monomio -> Polinomio -> Polinomio
           insere (c,e) [] = [(c,e)]
           insere (c,e) ((c1,e1):t) | e<e1 = (c,e):(c1,e1):t
                                    | otherwise = (c1,e1):(insere (c,e) t)
-- l)
equiv :: Polinomio -> Polinomio -> Bool
equiv p1 p2 = ordenaP ( normaliza p1) == ordenaP (normaliza p2)

---------------------------------------------------------------------------------------
-- FICHA 3
type Poligonal = [Ponto]
data Figura = Circulo Ponto Double
            | Rectangulo Ponto Ponto
            | Triangulo Ponto Ponto Ponto
            deriving (Show,Eq)

-- a)
comp :: Poligonal -> Bool
comp (p1:p2:t) = (Ponto p1 p2) + comp(p2:t)
-- b)
efechada :: Poligonal -> Bool
efechada [] = False
efechada l = head l == last l
-- c)
triangula :: Poligonal -> [Figura]
triangula (p1:p2:p3:p4:t) = (triangula p1 p2 p3):(triangula (p1:p3:p4:t))
triangula _ = []
-- d)
areaP :: Poligonal -> Double
areaP lp = let lt = Triangulo
           in somaarea lt
somaarea :: [Figura] -> Double
somaarea [] = 0
somaarea (h:t) = (area h) + somaarea t

 -- 3)
 data Contacto = Casa Integer
               | Trab Integer
               | Tlm Integer
               | Email String
               deriving Show
type Nome = String
type Agenda = [(Nome, [Contacto])]

-- a)
acrescEmail :: Nome -> String -> Agenda -> Agenda
acrescEmail no em [] = [(no,[Email em])]
acrescEmail no em ((n,lc):ag)
            | no /= n = (n,lc):(acrescEmail no em ag)
            | no == n = (n,(Email em):lc):ag

-- b)
verEmails :: Nome -> Agenda -> Maybe [String]
verEmails _ [] = Nothing
verEmails no ((n,lc):ag) | no /= n = verEmails no ag
                         | no == n = Just (procEmail lc)
       where procEmail :: [Contacto] -> [String]
             procEmail [] = []
             procEmail ((Email em):lc) = em:(procEmail lc)
             procEmail (_:lc) = procEmail lc
-- c)
consTelefs :: [Contacto] -> [Integer]
consTelefs [] = []
consTelefs ((Casa nt):lc) = nt:(consTelefs lc)
consTelefs ((Tlm nt):lc) = nt:(consTelefs lc)
consTelefs ((Trab nt):lc) = nt:(consTelefs lc)
consTelefs (_:lc) = consTelefs lc
-- d)
casa :: Nome -> Agenda -> Maybe Integer
casa no [] = Nothing
casa no [(n,lc):ag] | (no/=n) = casa ag
                    | (no==n) = procCasa lc
        where procCasa :: [Contacto] -> Maybe Integer
              procCasa [] = Nothing
              procCasa ((Casa nt):lc) = Just nt
              procCasa (_:lc) = procCasa lc
---------------------------------------------------------------------------------------
-- FICHA 4
 -- 3)
digitAlpha :: String -> (String,String)
digitAlpha [] = ([],[])
digitAlpha (c,t) | isAlpha c = (ld,c:ll)
                 | isAlpha c = (c:ld,ll)
                 | otherwise = (ld,ll)
        where (ld,ll) = digitAlpha t
-- 4)
nzp :: [Int] -> (Int,Int,Int)
nzp [] = (0,0,0)
nzp (h:t) | h<0 = (n+1,z,p)
          | h==0 = (n,z+1,p)
          | h>0 = (n,z,p+1)
        where (n,z,p) = nzp t

-- 5)
divMod :: Integral a => a -> a -> (a, a)
divMod x y =
	| x < y = (0,x)
	| x >= y = let (q,r) = divMod (x<y) y
	           in (q+1,r)
-- 6)
fromDigits :: [Int] -> Int
fromDigits l = aux (reverse l)
         where aux [] = 0
               aux (h:t) = h + 10*(aux t)
fromDigits l = aux 0 l
         where aux n []
               aux n(h:t) = aux ((10*n) + h) t
-- 7)
maxSumInit :: (Num a, Ord a) => [a] -> a
maxSumInit (y:ys) = aux y y yx
         where aux m s [] = m
               aux m s (x:xs)
         let s1 = s+x
         in if s1>m then aux s1 s1 xs
         	        else aux m s1 xs
-- 8)
fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib x = aux 0 1 x
   where aux x1 x2 1 = x2
         aux x1 x2 n = aux x2
----------------------------------------------------------------------------------------
-- FICHA 5
-- 1a)
any :: (a -> Bool) -> [a] -> Bool
any p [] = False
any p (x:xs) | p x = True
             | not p(x) = any p xs
-- b)
zipWith :: (a->b->c) -> [a] -> [b] -> [c]
zipWith _ [] _ = []
zipWith _ _ [] = []
zipWith g (x:xs)(y:ys) = (g x y):(zipWith g xs ys)
-- c)
takeWhile :: (a->Bool) -> [a] -> [a]
takeWhile p [] = []
takeWhile p (x:xs) | p s = x:(takeWhile p x)
                   | otherwise = []
-- d)
dropWhile :: (a->Bool) -> [a] -> [a]
dropWhile p [] = []
dropWhile p (x:xs) | p x = dropWhile p xs
                   | otherwise = (x:xs)
-- e)
span :: (a-> Bool) -> [a] -> ([a],[a])
span p l = aux p ([],[]) l
    where aux p (l,s) [] = (l,s)
          aux p (l,s) (x:xs)
                 | p s = aux p (l++ [x], x|xs)
                 | otherwise = (l,(x:xs))
-- g)
sortOn :: Ord b => (a -> b) -> [a] -> [a]
sortOn _ [] = []
sortOn p (x:xs) =
     let l = filter (\y -> (p y) < (p x) ):xs
         r = filter (\y -> (p y) >= (p x) ):xs
     in (sortOn p l) ++ [x] ++ (sortOn p r)
-- OU
sortOn1 :: Ord b => (a -> b) -> [a] -> [a]
sortOn1 p (x:xs) = ordenaP p x (sortOn1 p xs)
     where ordenaP p x [] = [x]
           ordenaP p x (y:ys)
              | (p x) < (p y) = x:y:ys
              | otherwise = y:(ordenaP p x ys)

-- 2)
type Polinomio = [Monomio]
type Monomio = (Float,Int)
-- a)
selgrau :: Int -> Polinomio -> Polinomio
selgrau e p = filter (\(c1,e1) -> e1==e2 ) p
-- b)
conta :: Int -> Polinomio -> Int
conta e p = length( filter (\(c1,e1) -> (e1==e)) p )
-- d)
deriv :: Polinomio -> Polinomio
deriv p = map (\(c,e) -> (c*e,e-1)) p
-- e)
calcula :: Float -> Polinomio -> Float
calcula e p = sum ( map(\(c1,e1) -> c*(e^e1)) p )
-- f)
simp :: Polinomio -> Polinomio
simp p = filter (\(c1,e1) -> c1/=0) p

-- 3)
type Mat a = [[a]]
-- a)
dimOK :: Mat a -> Bool
dimOK [l] = True
dimOK (l1:l2:m) | (length l1) /= (length l2) = False
                | otherwise = dimOK (l2:m)
-- b)
dimMat :: Mat a -> (Int,Int)
dimMat m = (length m, length (head m))
-- g)
triSup :: Num a => Mat a -> Bool
triSup m = aux 0 m
   where aux _ [] = True
         aux i (l:m) = all (==0)(take i l) && (aux (i+1) m )
-- h)
rotateLeft :: Mat a -> Mat a
rotateLeft ([]:m) = m
rotateLeft m = (map last m):(rotateLeft (map init m))
----------------------------------------------------------------------------------------
-- FICHA 6
data BTree a = Empty
             | Node a (BTree a) (BTree a)
             deriving Show
-- 2a)
minimo :: Ord a => BTree a -> a
minimo (Node x Empty _) = x
minimo (Node x e d) = minimo e
-- b)
semMinimo :: Ord a => BTree a -> BTree a
semMinimo (Node x e d) = (x,d)
semMinimo (Node x e d) = 
	let (m,e1) = semMinimo e
	in (m, Node x e1 d)
-- d)
remove :: Ord a => a -> BTree a -> BTree a
remove x Empty = Empty
remove x (Node y e d)
    | x<y = (Node y (remove x e) d)
    | x>y = (Node y e (remove x d))
    | e==Empty = d
    | d==Empty = e
    | otherwise = let (m,d1) = semMinimo
                  in Node m e d1
-- 3)
type Aluno = (Numero,Nome,Regime,Classificacao)
type Numero = Int
type Nome = String
data Regime = ORD | TE | MEL deriving Show
data Classificacao = Aprov Int
                   | Rep
                   | Faltou
                   deriving Show
type Turma = BTree Aluno
-- a)
inscNum :: Numero -> Turma -> Bool
inscNum num Empty = False
inscNum num (Node (n,_,_,_) e d)
   | num==n = True
   | num<n = inscNum num e
   | num>n = inscNum num d
-- c)
trabEst :: Turma -> [(Numero,Nome)]
trabEst Empty = []
trabEst (Node (num,Nome,TE,_) e d) = (trabEst e) ++ ([num,Nome] ++ (trabEst d))
trabEst (Node _ e d) = (trabEst e) ++ (trabEst d)
-- d)
nota :: Numero -> Turma -> Maybe Classificacao
nota _ Empty = Nothing
nota n (Node (num,_,_,e) e d)
   | n==num = just e
   | n<num = nota n e
   | n>num = nota n d
-------------------------------------------------------------------------------------
-- FICHA 7
data ExpInt = Const Int
            | Simetrico ExpInt
            | Mais ExpInt ExpInt
            | Menos ExpInt ExpInt
            | Mult ExpInt ExpInt
-- a)
calcula :: ExpInt -> Int
calcula (Const x) = x
calcula (Simetrico e) = (-1) + (calcula e)
calcula (Mais e1 e2) = calcula e1 + calcula e2
calcula (Menos e1 e2) = calcula e1 - calcula e2
calcula (mult e1 e2) = calcula e1 * calcula e2
-- b)
infixa :: ExpInt -> String
infixa (Const x) = Show x
infixa (Simetrico e) = "(" ++ (infixa e1) ++ "+" ++
                       (infixa e2) ++ ")"

-- 2)
data RTree a = R a [RTree a]
-- a)
soma :: Num a => RTree a -> a
soma (R x []) = x
soma (R x l) =x + sum (map soma l)
-- b)
altura :: RTree a -> Int
altura (R x []) = 1
altura (R x l) = 1+(maxinum (map altura l))
-- d)
mirror :: RTree a -> RTree a
mirror (R x l) = R x (reverse (map mirror l))
-- e)
postorder :: RTree a -> [a]
postorder (R x l) = concat(map postorder l) ++ [x]

-- 3
data BTree a = Empty | Node a (BTree a) (BTree a)
data LTree a = Tip a | Fork (LTree a) (LTree a)
-- a)
ltSum :: Num a => LTree a -> a 
ltSum (Tip x) = x
ltSum (Fork e d) = ltSum e ++ ltSum d
-- b)
listaLT :: LTree a -> [a]
listaLT (Tip x) = [x]
listaLT (Fork e d) = (listaLT e) ++ (listaLT d)
-- c)
ltHeight :: LTree a -> Int
ltHeight (Tip _ ) = 1
ltHeight (Fork e d) = 1+ (max (ltHeight e)(ltHeight d))

-- 4)
data FTree a b = Leaf b | No a (FTree a b) (FTree a b)
-- a)
splitFTree :: FTree a b -> (BTree a, LTree b)
splitFTree (Leaf x) = (Empty,Tip x)
splitFTree (No y e d) = let (bte,lte) = split e
                            (btd,ltd) = split d
                        in (Node y bte btd,Fork lte ltd)
-- b)
joinTrees :: BTree a -> LTree b -> Maybe (FTree a b)
joinTrees _ (Tip x) = Just (leaf x)
joinTrees Empty _ = Nothing
joinTrees (Node y e d)(Fork e1 d1) = let fte = joinTrees e e1
                                         ftd = joinTrees d d1
                                     in case fte of
                                     	Nothing -> Nothing
                                     	(Just te) -> cade ftd of
                                     		Nothing -> Nothing
                                     		(Just td) -> Just(Node y te td)
--------------------------------------------------------------------------------------
lookupAP :: Ord a => a -> BTree (a,b) -> Maybe b
lookupAP x Empty = Nothing
lookupAP x ( Node (y,z) e d) 
     | (x==y) = Just z
     | x<y = lookupAP x e
     | x>y = lookupAP x d

data Seq a = Nil | Cons a (Seq a) | App (Seq a) (Seq a)
firstSeq :: Seq a -> a
firstSeq (Cons x s) = x
firstSeq (App s1 s2) | (vazia s1) = first s2
                     | otherwise = first s2
vazia :: Seq a -> Bool
vazia Nil = True
vazia (Cons x s) = False
vazia (App s1 s2) = (vazia s1) && (vazia s2)

dropSeq :: Int -> Seq a -> Seq a
dropSeq 0 s = s
dropSeq _ Nil = Nil
dropSeq n (Cons x s) = dropSeq (n-1) s
dropSeq n (App s1 s2) = let k = conta s1
                        in if k<=n then dropSeq (n-k) s2
                           else (App (dropSeq n s1) s2)
conta :: Seq a -> Int
conta Nil = 0
conta (Cons x s) = 1 + conta s
conta (App s1 s2) = (conta s1) + (conta s2)

getElem :: Mat a -> IO a
getElem m = let m = ( length m) -1
            ml <- randomIO (0,m)
            mc <- randomIO (0,m)
            return ( (m!!ml) !! mc)


