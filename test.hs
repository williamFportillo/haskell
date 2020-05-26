
module Test where

import Data.Char


main :: IO ()
main = return ()
division :: Int -> Int -> Int
division x y = x `div` y

cuadrado :: Int -> Int
cuadrado x = x * x

triple :: Int -> Int
triple n = n * 3

sumaNumeros :: Int -> Int -> Int
sumaNumeros x y = x + y

divisible :: Int -> Int -> String
divisible x y = if (x `mod` y) == 0
    then "son Divisibles"
    else "no son divisibles"

esMayor :: Int -> Int -> String
esMayor x y = if x>y
    then "x es Mayor "
    else " y Es Mayor que "

mayorVeinte :: Int  -> String
mayorVeinte x= if x > 20
    then "Es mayor que 20"
    else "Es menor que 20" 


contarCifras lista = [ if x < 10 then "Una cifra " else "Dos cifras " | x <- lista, odd x]


vocales frase = [ x |x<-frase,  x `elem` ['a','e','i','o','u'] ]

sumarVoc frase = sum[1|x<-(vocales frase)]


ejercicio1 lista = [x*2|x<-lista]

ejercicio2 lista = [x|x<-lista, x `mod` 7 == 0 || x `mod` 3 == 0]

media3 x y z= (x+y+z)/3

sumaMonedas a b c d e = (a*1)+(b*2)+(c*5)+(d*10)+(e*20)

volumenEsfera r = (4/3)*3.1416*r^3

areaDeCoronaCircular r1 r2 = 3.1416 * (r2^2 - r1^2)

ultimaCifra x = rem x 10

maxTres x y z = maximum [x, y, z]

rota1 xs = tail xs ++ [head xs]

rota n xs = drop n xs ++ take n xs

rango xs = [minimum xs, maximum xs]

palindromo xs = if xs == reverse xs then True else False

interior xs = tail(init xs)

finales n xs = drop (length xs - n ) xs

segmento m n xs = drop  (m-1) (take n xs)

extremos n xs = take n xs ++  drop (length xs - n) xs

mediano x y z =  (x+y+z) `div` 3

tresIguales x y z = x == y && x == z

tresDistintos x y z = x /= y && y /= z && x /= z

cuatroIguales x y z u = x == u && tresIguales x y z

triangular a b c = a < (b + c) && b < (a + c) && c < (a+b)

divisionSegura x y = if y == 0 then 9999.0 else x/y 

modulo x y = sqrt(x^2 + y^2)

mayorRectangulo  xs  ys = if product xs >=  product ys then xs else ys

cuadrante x y = if x >= 0 &&  y  >= 0 then 1
                else if x < 0 &&  y  >= 0 then 2
                else if  x  < 0 &&  y  < 0 then 3
                else if x  >= 0 &&  y  < 0 then 4
                else  0

punto x y = (y,x)

simetricoH x y = (x,-1*y)

distancia x1 x2 y1 y2 = sqrt(((x1-y1)^2) + ((x2-y2)^2) )

puntoMedio x1 y1 x2 y2 = (((x1+x2)/2),(y1+y2)/2)

sumaComplejos x1 x2 y1 y2 = ((x1+y1),(x2+y2))

productoComplejo a b c d = ( (a*c) - (b*d) , (a*d) + (b*c) )

conjugado a b = (a, -1*b)

intercala xs ys = [xs !! 0 , ys !! 0, xs !! 1 , ys !! 1]

ciclo xs = last xs : init xs

numeroMayor a b = ((max a b)*10)  + min a b

area a b c = (a + b + c) / 2

formaReducida x = ((x!!0 )`div` 2, (x !! 1 )`div`2 )

sumarRacionales a b c d =  ( ( (a*d)`div`2 ) + ( (b*c)`div` 2 ) ,(b*d)`div`2)

productoRacional a b c d = ((a*c)`div`2,(b*d)`div`2)

sumaDeCuadrados x = sum[ n^2 | n<-[1..x]]

replica n x =  replicate n x

suma n = sum[1..n]

linea n = [suma (n-1)+1..suma n]

triangulo n = [ linea x | x <- [1..n]]

perfectos n =  [x|x<-[1..n], n `mod` x == 0] 

pitagoricas n = [(x,y,z) | x <- [1..n], y <- [1..n], z <- [1..n], x^2 + y^2 == z^2]

numeroDePares t  = sum[1|x<-t, x `mod` 2 == 0]

productoEscalar xs ys = sum[ x*y |(x,y)<-zip xs ys ]

sumaConsecutivos xs = [x+y | (x,y)<- zip xs (tail xs)]

posiciones n xs = [ y-1 | (x,y) <- zip xs [1..] , x == n,n `elem` xs]

nombres bd = [x |(x,_,_,_)<-bd]

musicos bd = [x | (x,m,_,_)<-bd, m == "Musica"]

seleccion bd m = [x | (x,m',_,_)<-bd, m == m']

musicos' bd = seleccion bd "Musica"

vivas ps a = [x | (x,_,a1,a2) <- ps, a1 <= a, a <= a2]
-- import Test.QuickCheck
potencia x 0 = 1
potencia x n = x*(potencia x (n-1))

dobleFactorial 0 = 1
dobleFactorial 1 = 1
dobleFactorial n = n*(dobleFactorial (n-2))

mcd a 0 = a
mcd a b = mcd b (a `mod` b)

elem' x [] = False
elem' x (y:ys) | x == y = True
               | otherwise = elem' x ys


last' [x] = x
last' (_:xs) = last' xs

concat' []=[] 
concat' (xs:xss) = xs ++ concat' xss

selecciona (x:_) 0 = x
selecciona (x:xs) n = selecciona xs (n-1)

take' 0 _ = []
take' _ [] = []
take' n (x:xs) = x : take' (n-1) xs

mezcla [] [] = []
mezcla xs [] = xs
mezcla [] ys = ys
mezcla (x:xs) (y:ys) = [x,y] ++ mezcla xs ys 

ordenada [] = True
ordenada [_] = True
ordenada (x:y:xs) = x <= y && ordenada (y:xs) 

sumaCuadrado 0 = 0
sumaCuadrado sr = (sr^2)+ sumaCuadrado (sr-1)

numeroBloquesR 0 = 0
numeroBloquesR n = (2*n) + numeroBloquesR (n-1)

sumaCuadradosImparesR 0 = 0
sumaCuadradosImparesR n | (n `mod` 2) /= 0 = (n^2) + sumaCuadradosImparesR (n-1)
                        | otherwise = sumaCuadradosImparesR (n-1)

esDigito x n | x == (n`mod`10) = True
             | n < 10 = False
             | otherwise = esDigito x (n `div` 10)

numeroDeDigitos x | x < 10 = 1
                  | otherwise = 1 + numeroDeDigitos(x `div` 10)


listaNumeroR xs = listaNumeroR' (reverse xs)

listaNumeroR' [x] = x
listaNumeroR' (x:xs) = x + 10*(listaNumeroR' xs) 

primerDigitoR n | n<10 = n
                | otherwise = primerDigitoR (n `div` 10)

ultimoDigito n = n `rem` 10

cuadradosC [] = []
cuadradosC (x:xs) =  [x^2] ++ cuadradosC xs

imparesC [] = [] 
imparesC (x:xs) | odd x = [x] ++ imparesC xs 
                | otherwise = imparesC xs

imparesCuadradosC xs = cuadradosC (imparesC xs)

sumaCuadradosImparesC xs = sum (cuadradosC(imparesC xs))

entreL m n = [m..n]

entreL' m n | m > n = []
            | otherwise =  [m]++entreL' (m+1) n

mitadParesC [] = []
mitadParesC (x:xs) | x `mod` 2 == 0 = [x `div` 2]++mitadParesC xs 
                   | otherwise = mitadParesC xs

enRangoC a b [] = []
enRangoC a b (x:xs)| x >= a && x <= b = [x]++enRangoC a b xs 
                    | otherwise = enRangoC a b xs

sumaPositivosC [] = 0
sumaPositivosC (x:xs) | x >= 0 = x + sumaPositivosC xs 
                      | otherwise = sumaPositivosC xs


sustituyeImpar [] = []
sustituyeImpar (x:xs) | odd x = (x+1): sustituyeImpar xs
                      | otherwise = x : sustituyeImpar xs


agarradoC xs = sum[(x-(x*0.10))| x <- xs, (x-(x*0.10))<=199]

factores n = [x|x<-[1..n],n `mod` x == 0]


primo n = factores n == [1,n]

factoresPrimo n = [x |x<-[1..n],n `mod` x == 0, primo x == True]

distanciaC [] [] = 0
distanciaC xs [] = 0
distanciaC []  ys = 0 
distanciaC (x:xs) (y:ys) | x /= y = 1 + distanciaC xs ys
                         | otherwise = distanciaC xs ys

sumaDigitosC xs = sum[digitToInt x| x <- xs, isDigit x]

mayusculaInicial xs = [if x == 1 then toUpper y else toLower y|(x,y)<-zip [1..] xs]
todasminusculas xs  = [toLower x|x<-xs]
titulo (x:xs) = mayusculaInicial x : [if (length(x))>=4 then (mayusculaInicial x) else (todasminusculas x) |x<-xs]


posicionesC xs y = [ z |(z,x)<-zip [0..] xs, x == y]



palabras cs = words cs

longitudes xss = [length(x)|x<-(palabras xss)] 

une xss = concat xss

reagrupa [] = []
reagrupa xs = take 4 xs : reagrupa (drop 4 xs)

inversas xss = map reverse xss

agrupa :: [a] -> [Int] -> [[a]]
agrupa [] _ = []
agrupa xs (n:ns) = take n xs : agrupa (drop n xs) ns 

fraseT :: [String] -> String
fraseT xs = unwords xs

clave :: String -> String
clave xss = fraseT (agrupa (une (inversas (reagrupa(une ( palabras xss))))) (reverse(longitudes xss))) 

ceros :: Int -> Int

ceros n | n `rem` 10 == 0 = 1 + ceros (n `div` 10)
        | otherwise = 0

takeWhiles :: (a -> Bool) -> [a] -> [a]
takeWhiles _ [] = []
takeWhiles p (x:xs) | p x = x:takeWhiles p xs 
                   | otherwise = []


dropWhiles :: (a -> Bool) -> [a] -> [a]
dropWhiles _ [] = []
dropWhiles p (x:xs) | p x = dropWhiles p xs
                    | otherwise = x:xs

concatR :: [[a]] -> [a]
concatR [] = []
concatR (x:xss) = x ++ concatR xss 

concatP :: [[a]] -> [a]
concatP = foldr (++) []


mediaL xs = (sum xs) `div` (length xs)

divideMedia (x:xs) = (filter (<(mediaL xs)) xs, filter (>(mediaL xs)) xs)

agrupaC [] = []
agrupaC xss | [] `elem` xss = []
            | otherwise = map head xss : agrupaC (map tail(xss)) 

superpar n | (n `rem` 10) < 10 && even n = True
           | odd n = False
           | otherwise = superpar (n `div` 10)

maximumR [x] = x
maximumR (x:y:xs) = max x (maximumR (y:xs))


minimumR [x] = x
minimumR (x:y:xs) = min x (minimumR (y:xs))

inversaR [] = []
inversaR xs = last xs : inversaR (init xs)

sumaR :: Num b => (a -> b) -> [a] -> b

sumaR f [] = 0
sumaR f (x:xs) = f x + sumaR f xs

mapR f [] = []
mapR f (x:xs) = f x : mapR f xs 

filterR f [] = []
filterR f (x:xs) | f x = x:filterR f xs
                 | otherwise = filterR f xs

sumallR [] = 0
sumallR (x:xss) = sum x + sumallR xss

borraR y [] = []
borraR y (x:xs) | y == x = borraR y xs
                | otherwise = x:borraR y xs



producto ys = foldr (*) 1 ys

colas [] = [[]]
colas (x:xs) = (x:xs) : colas xs 

cabezas [] = [[]]
cabezas (x:xs) = [x:y | y <- cabezas xs]


repite x = x:repite x

repiteC x = [ x | y<- [1..] ]

repiteFinita 0 _ = []
repiteFinita n x = x:repiteFinita (n-1) x

repiteFinitaC n x = [ x | y<-[1..n]]

repiteFinita' n x = take n (repite x)

ecoC xs = concat [ repiteFinita' y x | (y,x)<- zip [1..] xs]

potenciasMenores x y = takeWhile (<y) (map (x^) [1..])

agrupamiento n [] = []
agrupamiento n xs | n <= length xs = take n xs : agrupamiento n (drop n xs)
                  | otherwise = take n xs : agrupamiento n xs

divisores x = sum[ 1 | y <- [1..x], x `mod` y == 0 ]

saberPrimo x | divisores x == 2 = True
             | otherwise = False



esMuyCompuesto x =if (divisores x) > maximum[divisores y | y <- [1..x-1]] then True else False


primoTruncable x | x < 10  = saberPrimo x
                 | otherwise = saberPrimo x && primoTruncable (x `div` 10)

enteros = 0 : concat [ [-x, x] | x <- [1..]]

-- ordena en pares una lista por ejemplo: [1,2,3] = [(1,2), (1,3), (2,3)]
paresOrdenados [] = [] 
paresOrdenados (x:xs) = [(x,n) | n <- xs] ++ paresOrdenados xs

-- ordena en pares una lista utilizando repeat por ejemplo: [1,2,3] = [(1,2), (1,3), (2,3)]
paresOrdenados' [] = []
paresOrdenados' (x:xs) = zip (repeat x) xs ++ paresOrdenados' xs 

-- repite n veces la funcion en x ejemplo: 3 (*10) 5 = 5000
potenciaFunc 0 _ x = x
potenciaFunc n f x = potenciaFunc (n-1) f (f x)

-- repite n veces la funcion en x, sin recursion ejemplo: 3 (*10) 5 = 5000
potenciaFunc' n f x = last (take (n+1) (iterate f x))