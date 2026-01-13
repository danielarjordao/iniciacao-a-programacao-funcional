{-
  Módulo: Exemp
  Descrição: Módulo principal da Tarefa 5

  Demonstra:
  - Importação de módulo personalizado (Conv2)
  - Importação de módulo de biblioteca (Data.Char)
  - Uso de if-then-else
  - Composição de funções entre módulos
-}

module Exemp where

import Conv2
import Data.Char

-- Função que verifica se caractere é letra e retorna suas versões maiúscula/minúscula
-- Se for letra: retorna [minúscula, maiúscula]
-- Se não for letra: retorna lista vazia
conv :: Char -> [Char]
conv x = if isAlpha x then upperandlower x else []
