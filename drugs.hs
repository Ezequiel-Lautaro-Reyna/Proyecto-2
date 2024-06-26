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
data Zona = Arco | Defensa | Mediocampo | Delantera deriving (Eq, Bounded, Ord, Show)
data TipoReves = DosManos | UnaMano deriving (Eq, Bounded, Show)
data Modalidad = Carretera | Pista | Monte | BMX deriving (Eq, Bounded, Show)
data PiernaHabil = Izquierda | Derecha deriving (Eq, Bounded, Show)
-- Sinonimo --
type ManoHabil = PiernaHabil 
--Deportista es un tipo algebraico con constructores parametricos --
data Deportista = Ajedrecista | Ciclista Modalidad | Velocista Altura | Tenista TipoReves ManoHabil Altura | Futbolista Zona NumCamiseta PiernaHabil Altura  
    deriving (Eq, Show)
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
contarFutbolistas :: [Deportista] -> Zona -> Int
contarFutbolistas [] z = 0
contarFutbolistas ((Futbolista Arco _ _ _):xs) Arco = 1 + contarFutbolistas xs Arco
contarFutbolistas ((Futbolista Defensa _ _ _):xs) Defensa = 1 + contarFutbolistas xs Defensa
contarFutbolistas ((Futbolista Mediocampo _ _ _):xs) Mediocampo = 1 + contarFutbolistas xs Mediocampo
contarFutbolistas ((Futbolista Delantera _ _ _):xs) Delantera = 1 + contarFutbolistas xs Delantera
contarFutbolistas (_:xs) z = contarFutbolistas xs z
-- 4 e
estaEnZona:: Zona -> Deportista -> Bool
estaEnZona Arco (Futbolista Arco _ _ _) = True
estaEnZona Defensa (Futbolista Defensa _ _ _) = True
estaEnZona Mediocampo (Futbolista Mediocampo _ _ _) = True
estaEnZona Delantera (Futbolista Delantera _ _ _) = True
estaEnZona _ _ = False
contarFutbolistas':: [Deportista] -> Zona -> Int
contarFutbolistas' [] z = 0
contarFutbolistas' xs z = length (filter(estaEnZona z) (xs))
--Ejercicio 5.a--
sonidoNatural :: NotaBasica -> Int
sonidoNatural Do = 0
sonidoNatural Re = 2
sonidoNatural Mi = 4
sonidoNatural Fa = 5
sonidoNatural Sol = 7
sonidoNatural La = 9
sonidoNatural Si = 11
--Ejercicio 5.b--
data Alteracion = Bemol | Natural | Sostenido
--Ejercicio 5.c--
data NotaMusical = Nota NotaBasica Alteracion
--Ejercicio 5.d--
sonidoCromatico :: NotaMusical -> Int
sonidoCromatico (Nota n Sostenido) = (sonidoNatural n) + 1
sonidoCromatico (Nota n Bemol)     = (sonidoNatural n) - 1 
sonidoCromatico (Nota n Natural)   = (sonidoNatural n) + 0
--Ejercicio 5.e--
instance Eq NotaMusical where
    n1 == n2 = sonidoCromatico n1 == sonidoCromatico n2
--Ejercicio 5.f--
instance Ord NotaMusical where
    n1 <= n2 = sonidoCromatico n1 <= sonidoCromatico n2
--Ejercicio 6.a--
primerElemento:: [a] -> Maybe a 
primerElemento [] = Nothing
primerElemento (x:xs) = Just x
--Ejercicio 7.a.1--
data Cola = VaciaC | Encolada Deportista Cola     
    deriving (Show)
atender :: Cola -> Maybe Cola
atender VaciaC = Nothing
atender (Encolada y x) = Just x
--Ejercicio 7.a.2--
encolar :: Deportista -> Cola -> Cola
encolar z VaciaC = (Encolada z VaciaC)
encolar z (Encolada y x) = (Encolada y (encolar z x)) 
--Ejercicio 7.a.3--
busca :: Cola -> Zona -> Maybe Deportista
busca VaciaC _ = Nothing
busca (Encolada (Futbolista Arco x y z) cola) Arco = Just (Futbolista Arco n ph a)
busca (Encolada (Futbolista Defensa x y z) cola) Defensa = Just (Futbolista Defensa n ph a)
busca (Encolada (Futbolista Mediocampo x y z) cola) Mediocampo = Just (Futbolista Mediocampo n ph a)
busca (Encolada (Futbolista Delantera x y z) cola) Delantera = Just (Futbolista Delantera n ph a)
busca (Encolada dep cola) zona = busca cola zona
--Ejercicio 7.b--
{-
Se parece a Lista
-}
--Ejercicio 8.a--
--Ejercicio 8.b--