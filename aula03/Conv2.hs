{-
  Módulo: Conv2
  Descrição: Módulo auxiliar para Exemp.hs - Tarefa 5

  Demonstra função que retorna lista com maiúscula e minúscula de um caractere
-}

module Conv2 where

import Data.Char

-- Função que retorna lista com minúscula e maiúscula do caractere
-- Exemplo: upperandlower 'a' = ['a', 'A']
upperandlower :: Char -> [Char]
upperandlower c = [toLower c, toUpper c]
