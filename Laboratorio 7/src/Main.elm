module Main exposing(..)

--Ejercicio #1
type Arbol = Vacio | Constructor Int Arbol Arbol 


--Ejercicio #2
masUno: Arbol->Arbol
masUno arb = case arb of
    Vacio->Vacio
    Constructor x arb1 arb2 -> Constructor (x+1) (masUno arb1) (masUno arb2)


--Ejercicio #3
map: (Int->Int)->Arbol->Arbol
map f arb = case arb of
    Vacio->Vacio
    Constructor x arb1 arb2 -> Constructor (f x) (map f arb1) (map f arb2)


--Ejercicio #4
sum: Arbol->Int
sum arb = case arb of
    Vacio->0    
    Constructor x arb1 arb2 -> x + (sum arb1) + (sum arb2)


--Ejercicio #5
foldTree: (Int->Int->Int->Int)->Int->Arbol->Int
foldTree f inicial arb = case arb of
    Vacio->inicial
    Constructor x arb1 arb2 -> f x (foldTree f inicial arb1) (foldTree f inicial arb2) 


--Complementos utilizados para pruebas
xg = Constructor 3 (Constructor 1 Vacio Vacio) Vacio
mg = Constructor 9 (Constructor 7 (Constructor 5 Vacio Vacio) Vacio) (Constructor 2 Vacio Vacio)
mas2 x = x+2
mas x y z = x+y+z
por x y z =x*y*z