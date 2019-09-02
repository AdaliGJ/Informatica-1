module Main exposing (..)



--Ejercicio # 1
iFilter: Int -> List Int -> List Int
iFilter n ns = case ns of 
    []->[]
    b::bs-> if modBy n b == 0 then iFilter n bs else b::iFilter n bs 

--Ejercicio # 2
filter: (Int->Bool)->List Int->List Int
filter f xs =case xs of 
    []->[]
    b::bs ->if f b == True then b:: filter f bs else filter f bs 

--Ejercicio # 3
iZipWith: List Int -> List Int -> List Int
iZipWith xs ys = case (xs, ys) of
    ([], zs)-> []
    (hs, [])-> []
    (b::bs,c::cs)-> (b+c)::iZipWith bs cs

--Ejercicio # 4
zipWith: (Int->Int->Int)->List Int-> List Int-> List Int
zipWith f xs ys = case (xs,ys) of
    ([], zs)-> []
    (hs, [])-> []
    (b::bs,c::cs) -> (f b c)::(zipWith f bs cs) 





--Funciones Booleanas para utilizar 
esPrimo: Int -> Bool
esPrimo n =  primo 2 n 
primo  cont n = if n == 1 then False else if n == 2 then True else if modBy   cont  n  == 0 then False else if cont == n-1 then True else primo (cont + 1) n

esPar: Int -> Bool 
esPar n = 
    if n == 0 then True else if n == 1 then False else not(esPar (n-1))

esImpar: Int -> Bool 
esImpar n = 
    if n == 0 then False else if n == 1 then True else not(esImpar (n-1))