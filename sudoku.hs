import Data.Array
import Data.Char
import Data.List
--import Text.Printf
import System.IO
import System.Directory
import SudokuAux


type Sudoku = Array (Int,Int) Int
type Bloque = Int --Array (Int,Int) Int -- Sudoku más pequeño (3x3)
type Fila = Int 
type Columna = Int
type Valor = Int
type Posicion = (Fila,Columna)

main :: IO ()
main = do 
    putStrLn "\n ¡Bienvenido a la resolución de sudoku! "
    putStrLn "\n ¿Cual de estos sudokus desea resolver? "
    putStrLn "\t1. Dificultad fácil"
    putStrLn "\t2. Dificultad media "
    putStrLn "\t3. Dificultad dificil"
    putStrLn "\t4. Salir"
    putStr "\nEscriba el número asociado a la selección: "
    o <- getLine
    case o of "1" -> lecturaSudoku "sudoku-easy.txt" "sudoku-easy-sol.txt"
              "2" -> lecturaSudoku "sudoku-medium.txt" "sudoku-medium-sol.txt"
              "3" -> lecturaSudoku "sudoku-hard.txt" "sudoku-hard-sol.txt"
              _ -> main

lecturaSudoku::String->String->IO ()
lecturaSudoku sudoku res = do
    putStrLn "Comenzamos la carga del sudoku. \n"
    --la <- nos permite extraer el sudoku del IO Sudoku que nos devuelve leerSudoku
    sudo <- leerSudoku sudoku
    sol  <- leerSudoku res
    escrituraSudoku sudo 

    --usamos el readfile en lugar de print sudo para que nos imprima el txt que es mas legible que un array
    impresion <- readFile "escritura.txt"
    putStrLn impresion
    --print sol
    comienzo_resolucion sudo sol

comienzo_resolucion::Sudoku -> Sudoku ->  IO ()
comienzo_resolucion sudoku sol = do
    --preguntamos primero por las distintas opciones de resolver el sudoku: automatica o manual
    putStrLn "\n¿De que forma quiere resolver el sudoku?"
    putStrLn "\t1. Manual"
    putStrLn "\t2. Automatica"
    o <- getLine
    case o of "1" -> resolucion_manual sudoku sol 
              "2" -> resolucion_automatica sudoku sol
              _ -> comienzo_resolucion sudoku sol 

resolucion_manual::Sudoku->Sudoku -> IO()
resolucion_manual sudoku sol = do 
    --preguntamos por filas
    putStrLn "\nBienvenido a la resolucion manual del sudoku, continuemos."
    putStrLn ""
    sudo<- readFile "escritura.txt"
    putStrLn sudo
    --FUNCION IMPRIMIR SUDOKU
    putStrLn "\n Indique la fila: "
    fila <-darNumero
    --preguntamos por columnas
    putStrLn "\n Indique la columna: "
    columna <- darNumero
    let posicion = (fila, columna)
    --cases de: insertar numero / posibilidades / comprobar numero
    putStrLn "\n¿Que desea realizar en esta posicion? "
    putStrLn "\t1. Insertar nuevo numero"
    putStrLn "\t2. Ver posibilidades de numero"
    putStrLn "\t3. Comprobar valor"
    putStrLn "\t4. Comprobar sudoku"
    putStrLn "\t5. Mas informacion"
    o <- getLine
    case o of "1" -> insertar_numero sudoku sol posicion
              "2" -> verPosibilidades sudoku sol posicion --habria que hacer un sequence
              "3" -> comprobacionValor sudoku sol posicion
              "4" -> comprobacionSudoku sudoku sol
              "5" -> masInformacion sudoku sol posicion
              _ -> resolucion_manual sudoku sol


insertar_numero :: Sudoku->Sudoku -> Posicion->IO ()
insertar_numero sudoku sol posicion = do
    putStrLn "\nNuevo valor: \n"
    valor <- darValor
    let nuevoSudo = actualizaSudoku sudoku posicion valor
    escrituraSudoku nuevoSudo
    resolucion_manual nuevoSudo sol 

verPosibilidades::Sudoku->Sudoku->Posicion->IO()
verPosibilidades sudoku sol pos = do 
    let posib = posibilidades sudoku pos 
    let frase = "Las posibilidades son: " ++ (show posib) 
    putStrLn frase
   
    resolucion_manual sudoku sol
    
comprobacionValor::Sudoku->Sudoku->Posicion->IO()
comprobacionValor sudoku sol pos = do 
    let bool = comprobarValor sudoku sol pos 
    if bool then do 
        putStrLn "El valor es correcto"
        resolucion_manual sudoku sol
    else do
        putStrLn "El valor no es correcto, vuelve a intentarlo"
        resolucion_manual sudoku sol
        

comprobacionSudoku::Sudoku->Sudoku->IO()
comprobacionSudoku sudoku sol = do 
    let bool = comprobarSudoku sudoku sol 
    if bool then do 
        putStrLn "El sudoku es correcto.\n HEMOS TERMINADO EL SUDOKU POR TANTO CERRAMOS LA APLICACION"
        --resolucion_manual sudoku sol
    else do
        putStrLn "El sudoku no es correcto, vuelve a intentarlo"
        resolucion_manual sudoku sol

masInformacion::Sudoku->Sudoku->Posicion->IO()
masInformacion sudoku sol (x,y) = do
    
    putStrLn "\n¿Que informacion desea consultar? "
    putStrLn "\t1. Informacion sobre el bloque"
    putStrLn "\t2. Informacion sobre la fila "
    putStrLn "\t3. Informacion sobre la columna"
    putStrLn "\t4. Atras"
    o <- getLine
    case o of "1" -> valorBloque sudoku sol (x,y)
              "2" -> valorFila sudoku sol (x,y)
              "3" -> valorColumna sudoku sol (x,y)
              _   -> resolucion_manual sudoku sol 
    resolucion_manual sudoku sol


valorBloque:: Sudoku->Sudoku->Posicion->IO()
valorBloque sudoku sol (x,y) = do
    let bloque      = numBloque (x,y)
    let listaBloque = [1..9] \\ (valoresEnBloque sudoku bloque) 
    let frase = "Los valores que faltan en el bloque " ++ (show bloque) ++ " son: " ++ (show listaBloque)
    putStrLn frase
    resolucion_manual sudoku sol

valorFila:: Sudoku->Sudoku->Posicion->IO()
valorFila sudoku sol (x,y) = do
    let listaFila   = [1..9] \\( valoresEnFila sudoku x )
    let frase = "Los valores que faltan en la fila " ++ (show x) ++ " son: " ++ (show listaFila)
    putStrLn frase
    resolucion_manual sudoku sol

valorColumna:: Sudoku->Sudoku->Posicion->IO()
valorColumna sudoku sol (x,y) = do
    let listaColumna = [1..9] \\ (valoresEnColumna sudoku y)
    let frase = "Los valores que faltan en la columna " ++ (show y) ++ " son: "++(show listaColumna)
    putStrLn frase
    resolucion_manual sudoku sol

resolucion_automatica :: Sudoku -> Sudoku -> IO()
resolucion_automatica sud sol = do 
    escrituraSudoku sol
    impresion <- readFile "escritura.txt"
    putStrLn "\nEsta es la solucion del sudoku: \n"
    putStrLn impresion


 


