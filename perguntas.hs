myenumFromThenTo :: Int -> Int -> Int -> [Int]
myenumFromThenTo a b c | a<=b && a<c = a:myenumFromThenTo b (b+(b-a)) c
					   | c<= b && a>c = a:myenumFromThenTo (a-(b-1)) b c
					   | a == c = [a]
					   | otherwise = []

junta :: [a] -> [a] ->[a]
junta [] [] = []
junta [] a = a 
junta a [] = a
juta (x:xs) l = x:(junta xs l)

encontra :: [a] -> Int -> a 
encontra (x:xs) 0 = x
encontra (x:xs) a | length (x:xs) >= a = encontra xs (a-1)
				  
myreverse :: [a] -> [a]
myreverse [] = []
myreverse (x:xs) = myreverse xs ++ [x]

mytake :: Int -> [a] -> [a]
mytake 0 l = []
mytake a [] = []
mytake a (x:xs) | a>=length(x:xs) = (x:xs)
				| otherwise = x:mytake (a-1) xs

mydrop :: Int -> [a] -> [a]
mydrop 0 l = l 
mydrop a [] = []
mydrop a (x:xs) | a>=length(x:xs) = []
				| otherwise = mydrop (a-1) xs

myzip :: [a] -> [b] -> [(a,b)]
myzip [] l = []
myzip l [] = []
myzip (x:xs) (h:t) = (x,h) : myzip xs t

myelem :: Eq a => a -> [a] -> Bool
myelem a [] = False
myelem a (x:xs) | a==x = True
				| otherwise = myelem a xs

myreplicate :: Int -> a -> [a]
myreplicate 0 a = []
myreplicate a b = b:myreplicate (a-1) b

myintersperse :: a -> [a] -> [a]
myintersperse a [b] = [b] 
myintersperse a [] = []
myintersperse a (x:xs) = x:a: myintersperse a xs

myconcat :: [[a]] -> [a]
myconcat [] = []
myconcat (l:y) = l ++ myconcat y 

mytails :: [a] -> [[a]]
mytails [a] = [[a],[]]
mytails (x:xs) = (x:xs) : mytails xs

myisPrefixOf :: Eq a => [a] -> [a] -> Bool
myisPrefixOf [] a = True 
myisPrefixOf a [] = False 
myisPrefixOf (x:xs) (h:t) | x==h = myisPrefixOf xs t
						  | otherwise = False

myisSuffixOf :: Eq a => [a] -> [a] -> Bool 
myisSuffixOf [] [] = True 
myisSuffixOf [] l = True 
myisSuffixOf a [] = False
myisSuffixOf (x:xs) (h:g:t) | x==g = myisSuffixOf xs t
						  | otherwise = True

myisSubsequenceOf :: Eq a => [a] -> [a] -> Bool
myisSubsequenceOf l [] = False
myisSubsequenceOf [] l = True
myisSubsequenceOf (x:xs) (h:t) | x==h = myisSubsequenceOf xs (h:t)
							   | otherwise = myisSubsequenceOf (x:xs) t

myelemIndices :: Eq a => a -> [a] -> [Int]
myelemIndices a [] = []
myelemIndices a l = aux 0 a l 

aux :: Eq a => Int -> a -> [a] -> [Int]
aux a b [] = []
aux a b (x:xs) | b==x = a:aux (a+1) b xs
			   | otherwise = aux (a+1) b xs

mynub :: Eq a => [a] -> [a] 
mynub [] = []
mynub (x:xs) = aux2 x xs

aux2 :: Eq a => a -> [a] -> [a]
aux2 a [] = [a]
aux2 a (x:xs) | myelem a (x:xs) = aux2 x xs
			  | otherwise = a:aux2 x xs

mydelete :: Eq a => a -> [a] -> [a]
mydelete a [] = []
mydelete a (x:xs) | a==x = xs
				  | otherwise = x:mydelete a xs

mybarra :: Eq a => [a] -> [a] -> [a]
mybarra l [] = l 
mybarra [] l = []
mybarra (x:xs) (h:t) | x==h = mybarra xs t
					 | otherwise = x:mybarra xs (h:t)

myunion :: Eq a => [a] -> [a] -> [a]
myunion [] k = k
myunion l [] = l 
myunion (x:xs) (h:t) | myelem h (x:xs) = myunion (x:xs) t
					 | otherwise = h:myunion (x:xs) t

myintersect :: Eq a => [a] -> [a] -> [a]
myintersect [] l = []
myintersect l [] = []
myintersect (x:xs) (h:t) | myelem x (h:t) = x:myintersect xs (h:t)
						 | otherwise = myintersect xs (h:t)

myinsert :: Ord a => a -> [a] -> [a]
myinsert a [] = [a]
myinsert a (x:xs) | a<x = a:x:xs
				  | otherwise = x:myinsert a xs

myunwords :: [String] -> String
myunwords [x] = x
myunwords (x:xs) = x ++ " " ++ myunwords xs

myunlines :: [String] -> String
myunlines [a] = a ++ "\n"
myunlines (x:xs) = x ++ "\n" ++ myunlines xs

mypMaior :: Ord a => [a] -> Int
mypMaior [a] = 0 
mypMaior (x:xs) = aux3 0 (x:xs)

aux3 :: Ord a => Int -> [a] -> Int
aux3 a [b] = a
aux3 a (x:xs) | x < (head xs) = aux3 a (x:xs)  
			  | otherwise = aux3 (a+1) xs 

temRepetidos :: Eq a => [a] -> Bool
temRepetidos [] = False 
temRepetidos (x:xs) | myelem x xs = True 
					| otherwise = temRepetidos xs

algarismos :: [Char] -> [Char]
algarismos [] = []
algarismos (x:xs) | x>='0' && x<='9' = x:algarismos xs
				  | otherwise = algarismos xs

posImpares :: [a] -> [a]
posImpares [] = []
posImpares (x:xs) = aux4 0 (x:xs)

aux4 :: Int -> [a] -> [a]
aux4 a [] = []
aux4 a (x:xs) | mod a 2 == 0 = aux4 (a+1) xs 
		      | otherwise = x:aux4 (a+1) xs

posPares :: [a] -> [a]
posPares [] = []
posPares (x:xs) = aux5 0 (x:xs)

aux5 :: Int -> [a] -> [a]
aux5 a [] = []
aux5 a (x:xs) | mod a 2 == 0 = x:aux5 (a+1) xs 
		      | otherwise = aux5 (a+1) xs

isSorted :: Ord a => [a] -> Bool
isSorted [a] = True
isSorted (x:xs) | x<=head xs = isSorted xs
				| otherwise = False

menor :: String -> String -> Bool
menor [] l = True 
menor l [] = False 
menor (x:xs) (h:t) = menor xs t 

elemMSet :: Eq a => a -> [(a,Int)] -> Bool
elemMSet a [] = False 
elemMSet a ((c,b):xs) | a==c = True
					  | otherwise = elemMSet a xs

lengthMSet :: [(a,Int)] -> Int
lengthMSet [] = 0 
lengthMSet ((a,b):xs) = b + lengthMSet xs

converteMSet :: [(a,Int)] -> [a] 
converteMSet [] = []
converteMSet ((a,b):xs) | b==1 = a:converteMSet xs		
						| otherwise = a:converteMSet ((a,b-1):xs)

insereMSet :: Eq a => a -> [(a,Int)] -> [(a,Int)]
insereMSet a [] = [(a,1)]
insereMSet a ((c,b):xs) | a==c = ((c,b+1):xs)
						| otherwise = (c,b):insereMSet a xs

removeMSet :: Eq a => a -> [(a,Int)] -> [(a,Int)]
removeMSet a [] = []
removeMSet a ((b,c):xs) | a==b && c==1 = xs
						| a==b && c>1 = ((b,c-1):xs)
						| otherwise = (b,c):removeMSet a xs

constroiMSet :: Ord a => [a] -> [(a,Int)]
constroiMSet [] = []
constroiMSet (x:xs) = constroi x 1 (xs) 

constroi :: Ord a => a -> Int-> [a] -> [(a,Int)]
constroi a b [c] = [(a,b+1)]
constroi a b (x:xs) | a==x = constroi a (b+1) xs
					| otherwise = (a,b) : constroi x 1 xs 

mypartitionEithers :: [Either a b] -> ([a],[b]) 
mypartitionEithers [] = ([],[]) 
mypartitionEithers [Right a]  = ([],[a])
mypartitionEithers [Left a] = ([a],[])

mycatMaybes :: [Maybe a] -> [a]
mycatMaybes [Nothing] = [] 
mycatMaybes (Nothing:xs) = mycatMaybes xs
mycatMaybes (Just x :xs) = x:mycatMaybes xs

data Movimento = Norte | Sul | Este | Oeste
		deriving Show

posicao :: (Int,Int) -> [Movimento] -> (Int,Int)
posicao (a,b) [] = (a,b)
posicao (a,b) (Norte:xs) = posicao (a+1,b) xs
posicao (a,b) (Sul:xs) = posicao (a-1,b) xs
posicao (a,b) (Este:xs) = posicao (a,b+1) xs
posicao (a,b) (Oeste:xs) = posicao (a,b-1) xs

caminho :: (Int,Int) -> (Int,Int) -> [Movimento]
caminho (a,b) (c,d) | a==c && b==d = []	
					| a<c = Norte:caminho (a+1,b) (c,d)
					| a>c = Sul:caminho (a-1,b) (c,d)
					| b<d = Este:caminho (a,b+1) (c,d)
					| b>d = Oeste:caminho (a,b-1) (c,d)

vertical :: [Movimento] -> Bool 
vertical [] = True 
vertical (Norte:xs) = vertical xs
vertical (Sul:xs) = vertical xs
vertical (Este:xs) = False
vertical (Oeste:xs) = False

data Posicao = Pos Int Int
             deriving Show

maisCentral :: [Posicao] -> Posicao
maisCentral [a] = a 
maisCentral (Pos a b: Pos c d: xs) | modulo a<= modulo c && modulo b<= modulo d= maisCentral (Pos a b: xs)
								   | otherwise = maisCentral (Pos c d: xs)


modulo :: Int -> Int
modulo b | b>=0 = b
	     | b<0 = -b

constroiMSet :: Ord a => [a] -> [(a,Int)]
constroiMSet [] = []
constroiMSet (x:xs) = seila x (constroiMSet xs)

seila :: Ord a => a -> [(a,Int)] -> [(a,Int)]
seila a [] = [(a,1)]
seila a ((x,b):xs) | a==x = ((x,b+1):xs)
				   | otherwise = (x,b):seila a xs

concat :: [[a]] -> [a]
concat [] = []
concat ((x:xs):ys) = x:xs:ys

pMaior :: Ord a => [a] -> Int
pMaior [a] = 0
pMaior (x:xs) = numsei 0 (x:xs) 

numsei :: Ord a => Int -> [a] -> Int
numsei a [b] = a 
numsei a (x:y:xs) | x>y = numsei a (x:xs)
				  | otherwise = numsei (a+1) (y:xs) 


//ordem superior

//flip -> Troca a ordem dos argumentos de uma função binaria 
//zipWith constrói uma lista cujos elementos são calculados por uma função que é aplicada a argumentos que vêm de duas listas
//takeWhile recebe uma condição e uma lista e retorna o segmento inicial da lista cujos elementos satisfazem a condição dada
//dropWhile recebe uma condição e uma lista e retorna a lista sem o segmento inicial de elementos que satisfazem a condição dada
//foldr é uma função de ordem superior que recebe o operador f que é usado para construir o resultado, e o valor z a devolver quando a lista é vazia

myany :: (a->Bool) -> [a] -> Bool 
myany f [] = True
myany f (x:xs) | f x = myany f xs
			   | otherwise = False

myzipwith :: (a->b->c) -> [a] -> [b] -> [c]
myzipwith f [] [] = []
myzipwith f x [] = x
myzipwith f [] l = l 
myzipwith f (x:xs) (h:hs) = (f x h) : myzipwith f xs hs

mytakeWhile :: (a->Bool) -> [a] -> [a]
mytakeWhile f [] = []
mytakeWhile f (x:xs) | f x = f:mytakeWhile f xs
                     | otherwise = mytakeWhile f xs

mydropWhile :: (a->Bool) -> [a] -> [a]
mydropWhile f [] = []
mydropWhile f (x:xs) | f x = mydropWhile f xs
					 | otherwise = x:mydropWhile f xs

myspan :: (a->Bool) -> [a] -> ([a],[a])
myspan f [] = ([],[])
myspan f (x:xs) |                 


