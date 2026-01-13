{-
  Módulo: Conv1
  Descrição: Módulo de exemplo da Tarefa 5 - Importação de módulos

  Demonstra a importação do módulo Data.Char e uso de suas funções
-}

module Conv1 where

import Data.Char

-- Constante que usa toLower
-- toLower converte caractere para minúscula
con :: Char
con = toLower 'A'

-- Função que converte caractere para maiúscula
-- toUpper converte caractere para maiúscula
fun :: Char -> Char
fun x = toUpper x
