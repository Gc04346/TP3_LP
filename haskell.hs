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
 |otherwise = Falsev
 
rBinaria :: Integer -> [Integer]
rBinaria 0 = []
rBinaria n = rBinaria (n `div` 2) ++ [n `mod` 2]

main = do
    print $ primo 997