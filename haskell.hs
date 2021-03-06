pow :: Integer -> Integer -> Integer
pow n e
 |e==1 = n
 |otherwise = n * pow n (e-1)
 
somaImpares :: [Integer] -> Integer
somaImpares [] = 0
somaImpares (a:lista)
 |a `mod` 2 == 0 = 0 + somaImpares lista
 |otherwise = a + somaImpares lista

substituir :: Integer -> Integer -> [Integer] -> [Integer]
substituir sub new [] = []
substituir sub new (a:lista)
 |a == sub = new : substituir sub new lista
 |otherwise = [a] ++ substituir sub new lista

verificaPrimo :: Integer -> [Integer] -> Bool
verificaPrimo n [] = True
verificaPrimo n (a:lista)
 |n `mod` a == 0 = False
 |otherwise = verificaPrimo n lista

primo :: Integer -> Bool
primo n
 |n == 0 = False
 |n == 1 = True
 |n == 2 = True
 |otherwise = verificaPrimo n [2..(n-1)]
 
somaDivs :: Integer -> [Integer] -> Integer
somaDivs n [] = 0
somaDivs n (a:lista)
 |n `mod` a == 0 = a + somaDivs n lista
 |otherwise = somaDivs n lista
 
numPerfeito :: Integer -> Bool
numPerfeito n
 |somaDivs n [1..(n-1)] == n = True
 |otherwise = False
 
rBinaria :: Integer -> [Integer]
rBinaria 0 = []
rBinaria n = rBinaria (n `div` 2) ++ [n `mod` 2]

elemRepetido :: Integer -> [Integer] -> Bool
elemRepetido n [] = False
elemRepetido n (a:lista)
 | n == a = True
 |otherwise = elemRepetido n lista

elemDistintos :: [Integer] -> Bool
elemDistintos [] = True
elemDistintos (a:lista)
 |elemRepetido a lista == True = False
 |otherwise = elemDistintos lista
 
disjuntas :: [Integer] -> [Integer] -> Bool
disjuntas [] lista2 = True
disjuntas (a:lista1) lista2
 |elemRepetido a lista2 == False = disjuntas lista1 lista2
 |otherwise = True
 
espelhaLista :: [Integer] -> [Integer]
espelhaLista [] = []
espelhaLista (a:lista) = espelhaLista lista ++ [a]

verificaTrasPraFrente :: [Integer] -> [Integer] -> Bool
verificaTrasPraFrente [] [] = True
verificaTrasPraFrente (a:lista1) (b:lista2)
 | a /= b = False
 | otherwise = verificaTrasPraFrente lista1 lista2
 
palindromo :: [Integer] -> Bool
palindromo lista = verificaTrasPraFrente lista (espelhaLista lista)

soma :: Integer -> Integer -> Integer
soma a b = a+b

funcao :: Integer -> [Integer] -> [Integer]
funcao n [] = []
funcao n (a:lista) = n+a : funcao (soma n a ) lista

somaParciais :: [Integer] -> [Integer]
somaParciais (a:lista) = a : funcao a lista

linearizar :: [[Integer]] -> [Integer]
linearizar [] = []
linearizar (sublista:lista) = sublista ++ linearizar lista

shift :: Integer -> [Integer] -> [Integer]
shift 0 lista = lista
shift n (a:lista) = shift (n-1) (lista ++ [a])
final :: Integer -> [Integer] -> [Integer]
final 0 lista = lista
final n (a:lista) = final (n-1) lista

removerFim :: Integer -> [Integer] -> [Integer]
removerFim n lista = espelhaLista(final n (espelhaLista lista))

intercalar :: [Integer] -> [Integer] -> [Integer]
intercalar [] lista2 = lista2
intercalar lista1 [] = lista1
intercalar (e1:lista1) (e2:lista2)
 |e1 > e2 = e2 : intercalar (e1 : lista1) lista2
 |otherwise = e1 : intercalar lista1 (e2 : lista2)
 
quiosque :: Integer -> [Integer]
quiosque 0 = []
quiosque n
 |n - 100 >= 0 = quiosque (n-100) ++ [100]
 |n - 50 >= 0 = quiosque (n-50) ++ [50]
 |n - 20 >= 0 = quiosque (n-20) ++ [20]
 |n - 10 >= 0 = quiosque (n-10) ++ [10]
 |n - 5 >= 0 = quiosque (n-5) ++ [5]
 |n - 1 >= 0 = quiosque (n-1) ++ [1]