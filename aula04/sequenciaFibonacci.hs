module SequenciaFibonacci where

{-
    SEQUÊNCIA DE FIBONACCI COM zipWith e tail

    A sequência de Fibonacci é: 0, 1, 1, 2, 3, 5, 8, 13...
    Cada número é a soma dos dois anteriores.

    Como funciona:

    1. Começamos com 0 e 1 (os dois primeiros números)

    2. 'tail lista' pega a lista e remove o primeiro número
       Exemplo: tail [0, 1, 1, 2] = [1, 1, 2]

    3. 'zipWith (+)' soma duas listas, elemento por elemento
       Exemplo: zipWith (+) [0, 1, 1] [1, 1, 2] = [0+1, 1+1, 1+2] = [1, 2, 3]

    4. Juntando tudo: começamos com [0, 1] e vamos somando cada número com o próximo
       para criar o próximo número da sequência

    5. 'take (n + 1)' pega apenas os primeiros n+1 números da sequência,
       porque queremos incluir o 0 inicial.

    6. Como o Haskell sabe quando parar?

       A lista 'lista' continua para sempre: 0, 1, 1, 2, 3, 5, 8, 13, 21, ...
       Mas o Haskell usa "avaliação preguiçosa" (lazy evaluation):

       - O Haskell só calcula o que você realmente precisa!
       - Quando você pede 'take (n + 1) lista', ele calcula apenas n+1 elementos
       - Os outros elementos nunca são calculados (economiza tempo e memória)

       Exemplo: sequenciaFibonacci 6 -> take 7 lista -> calcula só 7 elementos e para!

       Isso é uma característica especial do Haskell: você pode trabalhar com
       listas "infinitas" sem travar o computador!

    Passo a passo para n = 6:

    lista = 0 : 1 : zipWith (+) lista (tail lista)

          = 0 : 1 : zipWith (+) (0 : 1 : ...) (1 : ...)
          = 0 : 1 : (0 + 1) : zipWith (+) (1 : ...) (...)
          = 0 : 1 : 1 : (1 + 1) : zipWith (+) (...) (...)
          = 0 : 1 : 1 : 2 : (1 + 2) : zipWith (+) (...) (...)
          = 0 : 1 : 1 : 2 : 3 : (2 + 3) : zipWith (+) (...) (...)
          = 0 : 1 : 1 : 2 : 3 : 5 : (3 + 5) : zipWith (+) (...) (...)
          = 0 : 1 : 1 : 2 : 3 : 5 : 8 : ...

    A cada passo, pegamos os dois últimos números e somamos para criar o próximo.

    Resultado final: sequenciaFibonacci 6 = [0, 1, 1, 2, 3, 5, 8]

-}

sequenciaFibonacci :: Int -> [Int]
sequenciaFibonacci n = take (n + 1) lista
  where
    lista = 0 : 1 : zipWith (+) lista (tail lista)



main :: IO ()
main = do
    print (sequenciaFibonacci 10)
