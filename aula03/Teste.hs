{-
  Módulo: Teste
  Descrição: Módulo de exemplo para a Tarefa 3 da Ficha Prática 1

  Este módulo contém duas funções simples para demonstrar:
  - Criação de módulos em Haskell
  - Inferência de tipos
  - Composição de funções
-}

module Teste where

-- Função funcao1
-- Tipo: Num a => a -> a -> a
-- Recebe dois números e retorna x + (70 * y)
-- Exemplo: funcao1 5 2 = 5 + (70 * 2) = 5 + 140 = 145
funcao1 :: Num a => a -> a -> a
funcao1 x y = x + (70 * y)

-- Função ex
-- Tipo: Num a => a -> a
-- Recebe um número e multiplica por 50
-- Exemplo: ex 10 = 50 * 10 = 500
ex :: Num a => a -> a
ex a = 50 * a

{-
  Exemplos de uso no GHCi:

  > :l Teste.hs
  > ex 10
  500

  > funcao1 5 2
  145

  > funcao1 (ex 10) 1
  570

  Explicação da última expressão:
  1. ex 10 = 50 * 10 = 500
  2. funcao1 500 1 = 500 + (70 * 1) = 500 + 70 = 570
-}
