module SudokuAux 
    (actualizaSudoku,
    comprobarValor, 
    leerSudoku,
    comprobarSudoku, 
    cargaSudoku, 
    posibilidades,
    darNumero,
    darValor,
    escrituraSudoku,
    valoresEnBloque, valoresEnColumna, valoresEnFila,
    numBloque
    ) where


import Data.Array
import Data.Char
import Data.List
--import Text.Printf
import System.IO
import System.Directory


type Sudoku = Array (Int,Int) Int
type Bloque = Int -- Sudoku más pequeño (3x3)
type Fila = Int 
type Columna = Int
type Valor = Int
type Posicion = (Fila,Columna)

cargaSudoku :: String -> Sudoku
cargaSudoku s = listArray ((1,1),(9,9)) (map read (words s)) 

actualizaSudoku::Sudoku->Posicion->Valor->Sudoku
actualizaSudoku sudoku pos val = sudoku // [(pos,val)]

comprobarValor :: Sudoku -> Sudoku -> Posicion -> Bool
comprobarValor sudoku sol pos = sudoku!pos == sol!pos

comprobarSudoku :: Sudoku -> Sudoku -> Bool
comprobarSudoku sudoku sol = sudoku == sol

leerSudoku :: String -> IO Sudoku
leerSudoku fichero = do
    existe <- doesFileExist fichero
    contenido <- readFile fichero
    let sudoku = cargaSudoku contenido
    return sudoku 

--FUNCION AUX QUE NOS PERMITE PASAR UN SUDOKU A UN STRING PARA POSTERIORMENTE
--ESCRIBIRLO EN UN ARCHIVO
sudoku2Lista::Sudoku->[Int] 
--tengo que usar el reverse para que en cambio me permita pasarlo bien
sudoku2Lista sudoku = (elems sudoku)

--mediante listaElementos tendremos cada numero con su espacio listo para pasarlo a un unico string
listaElementos::[Int]->[String]
listaElementos xs = [(show x )++ " " | x<-xs]

--queremos que se produzca un salto de linea cada vez que llegue al ultimo elemento de la columna
--por tanto debemos pasar un acc, el max de la columna que usaremos como mod
--nuestro caso base sera cuando terminemos con la lista
--el acc debe empezar siempre en 1 o en el inicio del sudoku
saltoLinea::[String]->Columna->Int-> String
saltoLinea [] _ _= ""
saltoLinea (x:xs) col acc 
--  |acc == (fi*col) = ""
  |acc`mod` col == 0 = x ++"\n"++ saltoLinea xs col (acc+1)
  |otherwise = x ++ saltoLinea xs col (acc+1)


--en esta funcion procedemos a copiar el sudoku en un archivo
escrituraSudoku ::Sudoku-> IO()
escrituraSudoku sudoku = do 
  let (_,(fi,col)) = bounds sudoku
  let elementos = sudoku2Lista sudoku
  let stringElementos = listaElementos elementos
  let salto = saltoLinea stringElementos col 1
  writeFile "escritura.txt" salto

enteroValido :: String -> Bool
enteroValido xs = elem xs lista
  where lista =["1","2","3","4","5","6","7","8","9"]


darNumero :: IO Fila
darNumero = do
    fi <- getLine
    if enteroValido fi then do
        let fila = read fi :: Fila 
        return fila
    else do
        putStrLn "Numero no valido, vuelve a dar un numero"
        darNumero


valorValido :: String -> Bool
valorValido xs = elem xs lista
  where lista =["0","1","2","3","4","5","6","7","8","9"]

darValor :: IO Valor
darValor = do
    putStr "\n Indique un numero: "
    fi <- getLine
    if valorValido fi then do
        let fila = read fi :: Fila 
        return fila
    else do
        putStrLn "Valor no valido, vuelve a dar un valor"
        darValor

posibilidades :: Sudoku -> Posicion -> [Valor] 
posibilidades sudoku (x,y) = [ z | z <- [1..9] , notElem z (valoresEnFila sudoku x) , notElem z (valoresEnColumna sudoku y), notElem z (valoresEnBloque sudoku (numBloque (x,y)))]

--Devuelve una lista con los valores de cada fila para comprobar si son unicos en cada fila
valoresEnFila :: Sudoku -> Fila -> [Valor]
valoresEnFila sudoku fi = [sudoku!(x,y) | x <- [1..fila], y <- [1..col] , x==fi]
  where (_,(fila,col)) = bounds sudoku

--Devuelve una lista con los valores de cada columna para comprobar si son unicos en cada columna
--el filtro actuara de forma que solo sacaremos los valores del bloque en el que nos encontremos
valoresEnColumna :: Sudoku -> Fila -> [Valor]
valoresEnColumna sudoku co = [sudoku!(x,y) | x <- [1..fila], y <- [1..col] , y==co]
  where (_,(fila,col)) = bounds sudoku

--Devuelve una lista con los valores de cada bloque para comprobar si son unicos en cada bloque
valoresEnBloque :: Sudoku -> Bloque -> [Valor]
valoresEnBloque sudoku bloque = [ sudoku!(x,y) | x <- [1..fila], y <- [1..col], numBloque (x,y) == bloque]
  where (_,(fila,col)) = bounds sudoku

-- f calcula el ultimo bloque de la anterior fila y se suma al resultado de c (que divide en 3 ya que cada 3 columnas hay un bloque nuevo).
numBloque :: Posicion -> Bloque
numBloque (x,y) 
    | (x >=1 && x<=9) && (y>=1 && y<=9) = f + c
    | otherwise = error "Bloque no valido"
    where f = (x-1) - (mod (x-1) 3) 
          c = round $fromIntegral (y+1)/3
          

--Devuelve una lista con las posiciones vacia para comprobar si una posicion es vacia
posicionVacia :: Sudoku -> [Posicion]
posicionVacia sudoku = [ (x,y) | x <- [1..fila], y <- [1..col], sudoku!(x,y)==0]
  where (_,(fila,col)) = bounds sudoku

--Devuelve si la posicion es vacia 
esPosicionVacia :: Sudoku -> Posicion -> Bool
esPosicionVacia sudoku (x,y) = elem (x,y) (posicionVacia sudoku)


 
