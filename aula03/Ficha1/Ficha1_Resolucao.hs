{-
Ficha Prática 1 — Resolução Oficial (Haskell)

Notas:
- Algumas tarefas pedem comandos no GHCi (não são "código" a compilar). Esses comandos
  estão nos comentários das respetivas secções.
- Na Tarefa 5, a ficha usa módulos separados (Conv1, Conv2, Exemp). Como este ficheiro é único,
  incluo o código desses módulos em blocos comentados. Para testar no GHCi como na ficha,
  copie cada bloco para o respetivo ficheiro (Conv1.hs, Conv2.hs, Exemp.hs).

Como executar:
- macOS/Linux:
    runghc Ficha1_Resolucao.hs
    ghci Ficha1_Resolucao.hs
- Windows (PowerShell):
    runghc .\Ficha1_Resolucao.hs
    ghci .\Ficha1_Resolucao.hs
-}

module Ficha1_Resolucao where

import Data.Char (toLower, toUpper, isAlpha)

--------------------------------------------------------------------------------
-- Tarefa 1 (no GHCi) — comandos sugeridos (executar no GHCi)
--------------------------------------------------------------------------------
{-
:set +t
fst (4,'a')
snd (4,'a')
fst (5.6,3)
:i fst
:t fst
:i tail
:t tail
tail [6,7,3,9]
tail "sdferta"
-}

--------------------------------------------------------------------------------
-- Tarefa 2 — inferir tipo e avaliar (exemplos)
--------------------------------------------------------------------------------
-- 1) [True, (5>4), (not ('5'=='6')), (True || (False && True))]
--    => [True, True, True, True] :: [Bool]

-- 2) ((tail "abcdef"), (head "abcdef"))
--    => ("bcdef",'a') :: (String, Char)

-- 3) [(tail "abcdef"), (head "abcdef")]
--    => ERRO de tipos (String vs Char) numa lista

-- 4) [4,5,6] ++ [3,5,8]  => [4,5,6,3,5,8]

-- 5) tail [6,7] => [7]

-- 6) concat ["asdf","bbb","tyuui","cccc"] => "asdfbbbtyuuicccc"

--------------------------------------------------------------------------------
-- Tarefa 3 — módulo Teste (incluído aqui como funções equivalentes)
--------------------------------------------------------------------------------
funcao1 :: Num a => a -> a -> a
funcao1 x y = x + (70*y)

ex :: Num a => a -> a
ex a = 50 * a

-- Exemplo (no GHCi): funcao1 (ex 10) 1  ==> 570

--------------------------------------------------------------------------------
-- Tarefa 4 — definir e testar funções
--------------------------------------------------------------------------------

-- 1) Recebe dois pares e devolve (soma dos 1os, produto dos 2os)
f1 :: (Int,Int) -> (Int,Int) -> (Int,Int)
f1 (a,b) (c,d) = (a+c, b*d)

-- 2) Devolve (maior, segundoMaior) de 3 inteiros
maior2 :: Int -> Int -> Int -> (Int,Int)
maior2 x y z =
  let m = max x (max y z)
      s = if m == x then max y z
          else if m == y then max x z
          else max x y
  in (m,s)

-- 3) Ordenar um triplo por ordem decrescente
ord3 :: (Int,Int,Int) -> (Int,Int,Int)
ord3 (a,b,c) =
  let m1 = max a (max b c)
      m3 = min a (min b c)
      m2 = a + b + c - m1 - m3
  in (m1,m2,m3)

-- 4) Verificar se 3 inteiros podem formar um triângulo
triangulo :: Int -> Int -> Int -> Bool
triangulo a b c = (a+b>c) && (a+c>b) && (b+c>a)

-- 5) abrev: primeiro nome + último apelido (usa words/unwords)
abrev :: String -> String
abrev nome =
  let ws = words nome
  in case ws of
      []  -> ""
      [x] -> x
      _   -> head ws ++ " " ++ last ws

--------------------------------------------------------------------------------
-- Tarefa 5 — módulos e imports (código a separar por ficheiros, se necessário)
--------------------------------------------------------------------------------
{-
-- Conv1.hs
module Conv1 where
import Data.Char

con = toLower 'A'
fun x = toUpper x

-- Conv2.hs
module Conv2 where
import Data.Char

upperandlower c = [toLower c, toUpper c]

-- Exemp.hs
module Exemp where
import Conv2
import Data.Char

conv x = if isAlpha x then upperandlower x else []
-}

--------------------------------------------------------------------------------
-- Tarefa 6 — consultar módulo Char (atual: Data.Char) no GHCi
--------------------------------------------------------------------------------
{-
:browse Data.Char
-}

--------------------------------------------------------------------------------
-- Tarefa 7 — recursividade
--------------------------------------------------------------------------------

-- 1) pot x y: calcula x^y (y >= 0) sem usar (^) da Prelude
pot :: Int -> Int -> Int
pot x y =
  if y == 0 then 1
  else x * pot x (y - 1)

-- 2) par com o primeiro e o último elemento (lista não vazia)
primUlt :: [a] -> (a,a)
primUlt xs = (head xs, last xs)

-- 3) par com a lista e o seu comprimento
listaEComp :: [a] -> ([a], Int)
listaEComp xs = (xs, length xs)

-- 4) média de uma lista (não vazia)
media :: [Float] -> Float
media xs = sum xs / fromIntegral (length xs)

--------------------------------------------------------------------------------
-- Pequeno main opcional (para testar rapidamente com runghc)
--------------------------------------------------------------------------------
main :: IO ()
main = do
  putStrLn "Ficha1_Resolucao.hs carregada com sucesso."
  putStrLn "Exemplos rápidos:"
  print (f1 (1,2) (3,4))            -- (4,8)
  print (maior2 10 5 7)             -- (10,7)
  print (ord3 (3,9,1))              -- (9,3,1)
  print (triangulo 3 4 5)           -- True
  print (abrev "Ana Maria Silva")   -- "Ana Silva"
  print (pot 2 10)                  -- 1024
  print (primUlt [1,2,3,4])         -- (1,4)
  print (listaEComp [10,20,30])     -- ([10,20,30],3)
  print (media [10,20,30,40])       -- 25.0
