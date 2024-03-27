{-# LANGUAGE NPlusKPatterns #-}
--Proyecto 2--
--Ejercicio 1.a--
data Carrera = Matematica | Fisica | Computacion | Astronomia
--Ejercicio 1.b--
titulo :: Carrera -> String
titulo Matematica = "Lic. en Matematica"
titulo Fisica = "Lic. en Fisica"
titulo Computacion = "Lic. en Computacion"
titulo Astronomia = "Lic. en Astronomia"
{-
Ejemplos

*Main> titulo Fisica
"Lic. en Fisica"
*Main> titulo Astronomia
"Lic. en Astronomia"
-}
--Ejercicio 1.c y 2.a-- 
data NotaBasica = Do | Re | Mi | Fa | Sol | La | Si deriving (Bounded, Eq, Ord, Show, Enum)
{-
Ejemplos

*Main> Do <= Re
True
*Main> Fa `min` Sol
Fa
-}
--Ejercicio 1.d--
cifradoAmericano :: NotaBasica -> Char
cifradoAmericano Do = 'C'
cifradoAmericano Re = 'D'
cifradoAmericano Mi = 'E'
cifradoAmericano Fa = 'F'
cifradoAmericano Sol = 'G'
cifradoAmericano La = 'A'
cifradoAmericano Si = 'B'
{-
Ejemplos

*Main> cifradoAmericano Do
'C'
*Main> cifradoAmericano Sol
'G'
-}
--Ejercicio 3.a--
minimoElemento :: (Eq a, Ord a, Show a) => [a] -> a 
minimoElemento [x] = x
minimoElemento (x:y:xs) | x <= y = minimoElemento (x:xs)
                        | x > y  = minimoElemento (y:xs)
{-
Ejemplos

*Main> minimoElemento [1]
1
*Main> minimoElemento [Re, Fa, Sol, Do]
Do
-}
--Ejercicio 3.b--
minimoElemento' :: (Eq a, Bounded a, Ord a, Show a, Enum a) => [a] -> a 
minimoElemento' [] = maxBound
minimoElemento' (x:y:xs) | x <= y = minimoElemento (x:xs)
                        | x > y  = minimoElemento (y:xs)
{-
Ejemplos

*Main> minimoElemento' []
()
*Main> minimoElemento' ([99,6,5,2] :: [Int])
2

Sin embargo este codigo no funciona para un Float
-}
--Ejercicio 3.c--
{-
Por la def de l tipo creado NotaBasica, ya se cumple el inciso 3.c.

Def. NotaBasica:  data NotaBasica = Do | Re | Mi | Fa | Sol | La | Si deriving (Bounded, Eq, Ord, Show)

Ord en este caso y la forma en la que los elementos estan cargados ayuda a definir esta funcion por si sola.
-}
--Ejercicio 4.a--
-- Sinonimos de tipo --
type Altura = Int
type NumCamiseta = Int
-- Tipos algebraicos sin parametros (aka enumerados) --
data Zona = Arco | Defensa | Mediocampo | Delantera deriving (Eq, Bounded, Ord)
data TipoReves = DosManos | UnaMano deriving (Eq, Bounded)
data Modalidad = Carretera | Pista | Monte | BMX deriving (Eq, Bounded)
data PiernaHabil = Izquierda | Derecha deriving (Eq, Bounded)
-- Sinonimo --
type ManoHabil = PiernaHabil 
--Deportista es un tipo algebraico con constructores parametricos --
data Deportista = Ajedrecista | Ciclista Modalidad | Velocista Altura | Tenista TipoReves ManoHabil Altura | Futbolista Zona NumCamiseta PiernaHabil Altura  deriving Eq
--Ejercicio 4.b--
{-
Es de tipo algebraico sin parametros
-}
--Ejercicio 4.c--
contar_velocistas :: [Deportista] -> Int
contar_velocistas [] = 0
contar_velocistas (Velocista _ : xs) = 1 + contar_velocistas xs
contar_velocistas ( _ : xs) = contar_velocistas xs
--Ejercicio 4.d--
contar_futbolistas :: [Deportista] -> Zona -> Int
contar_futbolistas [] z = 0
contar_futbolistas (Futbolista x _ _ _ :xs) z | x == z = 1 + contar_futbolistas xs z 
                                              | otherwise = contar_futbolistas xs z