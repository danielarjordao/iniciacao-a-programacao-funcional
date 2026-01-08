{-
    Outra implementação da função Fibonacci sem utilizar a recursividade
    Utilizando a implementação linear com acumuladores em Haskell.
-}

fib2 :: Int -> Int
fib2 n = fibAux n 0 1                        -- Chama a função auxiliar com os valores iniciais 0 e 1
  where
    fibAux 0 a _ = a                         -- Caso base: se n é 0, retorna o acumulador 'a'
    fibAux k a b = fibAux (k - 1) b (a + b)  -- Passa para o próximo número, atualizando os acumuladores

{-
    n é o índice da sequência de Fibonacci que queremos calcular.
    a é o valor atual da sequência de Fibonacci (inicialmente 0).
    b é o próximo valor da sequência de Fibonacci (inicialmente 1).

   quando n chega a 0, retornamos o valor de a, que é o n-ésimo número da sequência de Fibonacci.
   Caso contrário, chamamos fibAux novamente, decrementando n em 1, atualizando a para b (o próximo número na sequência)
   e atualizando b para a + b (a soma dos dois últimos números na sequência).
-}

{-
    Passo a passo da função fib2 para n = 6:
    fib2 6 = fibAux 6 0 1
           = fibAux (6 - 1) 1 (0 + 1)
           = fibAux 5 1 1
           = fibAux (5 - 1) 1 (1 + 1)
           = fibAux 4 1 2
           = fibAux (4 - 1) 2 (1 + 2)
           = fibAux 3 2 3
           = fibAux (3 - 1) 3 (2 + 3)
           = fibAux 2 3 5
           = fibAux (2 - 1) 5 (3 + 5)
           = fibAux 1 5 8
           = fibAux (1 - 1) 8 (5 + 8)
           = fibAux 0 8 13
           = 8
-}

main :: IO ()
main = do
    print (fib2 0)  -- Caso de teste 1: Deve retornar 0
    print (fib2 1)  -- Caso de teste 2: Deve retornar 1
    print (fib2 4)  -- Caso de teste 3: Deve retornar 3
