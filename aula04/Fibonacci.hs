{-
    Fazer a sequência de Fibonacci com recursividade em Haskell sem utilizar inteligência artificial.

    fib 0 = 0
    fib 1 = 1
    fib n = fib (n-1) + fib (n-2)

    Sequência de Fibonacci:
    0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, ...

    Trazer 3 casos de testes.
-}

{-
    Minha solução em C para adaptar para Haskell:

    int	ft_fibonacci(int index)
    {
        if (index < 0)
            return (-1);
        if (index == 0)
            return (0);
        if (index == 1 || index == 2)
            return (1);
        else
            return (ft_fibonacci(index - 1) + ft_fibonacci(index - 2));
    }
-}

fib :: Int -> Int
fib n
    | n == 0    = 0
    | n == 1    = 1
    | otherwise = fib (n - 1) + fib (n - 2)

{-
    Passo a passo da função fib para n = 4:
    fib 4 = fib (4 - 1) + fib (4 - 2)
          = fib 3 + fib 2
          = (fib (3 - 1) + fib (3 - 2)) + (fib (2 - 1) + fib (2 - 2))
          = (fib 2 + fib 1) + (fib 1 + fib 0)
          = ((fib (2 - 1) + fib (2 - 2)) + 1) + (1 + 0)
          = ((fib 1 + fib 0) + 1) + (1 + 0)
          = ((1 + 0) + 1) + (1 + 0)
          = (1 + 1) + 1
          = 2 + 1
          = 3

    Passo a passo da função fib para n = 6:
    fib 6 = fib (6 - 1) + fib (6 - 2)
          = fib 5 + fib 4
          = (fib 4 + fib 3) + (fib 3 + fib 2)
          = ((fib 3 + fib 2) + (fib 2 + fib 1)) + ((fib 2 + fib 1) + fib 1)
          = (((fib 2 + fib 1) + (fib 1 + fib 0)) + (fib 1 + fib 0 + 1)) + ((fib 1 + fib 0 + 1) + 1)
          = (((1 + 0) + (1 + 0)) + (1 + 0 + 1)) + ((1 + 0 + 1) + 1)
          = ((1 + 1) + (1 + 1)) + (2 + 2)
          = (2 + 2) + 4
          = 4 + 4
          = 8
-}

-- Casos de teste
main :: IO ()
main = do
    putStrLn "Testes da função fib:"
    let test1 = fib 0
        -- Deve retornar 0 pois a posição 0 da sequência é 0
    putStrLn $ "Fib 0: " ++ show test1
    let test2 = fib 1
        -- Deve retornar 1 pois a posição 1 da sequência é 1
    putStrLn $ "Fib 1: " ++ show test2
    let test3 = fib 4
        -- Deve retornar 3 pois a posição 4 da sequência é 3
    putStrLn $ "Fib 4: " ++ show test3
    let test4 = fib 6
        -- Deve retornar 8 pois a posição 6 da sequência é 8
    putStrLn $ "Fib 6: " ++ show test4

{-
    EXPLICAÇÕES SOBRE putStrLn E show:

    putStrLn :: String -> IO ()
    - Função que imprime uma string no console e adiciona uma quebra de linha no final
    - Tipo: recebe uma String e retorna uma ação IO () (sem valor de retorno)
    - Exemplo: putStrLn "Olá" imprime "Olá" e pula linha

    show :: Show a => a -> String
    - Função que converte valores de tipos que implementam a type class Show em String
    - Tipo: recebe qualquer tipo que seja Show e retorna uma String
    - Exemplo: show 42 retorna "42", show True retorna "True"
    - É necessário porque putStrLn só aceita String, não aceita Int, Bool, etc.

    (++) :: [a] -> [a] -> [a]
    - Operador de concatenação de listas (String é lista de Char)
    - Exemplo: "Olá" ++ " " ++ "Mundo" resulta em "Olá Mundo"

    ($) :: (a -> b) -> a -> b
    - Operador de aplicação de função com baixa precedência
    - Evita o uso excessivo de parênteses
    - putStrLn $ "fib 0: " ++ show (fib 0)
      é equivalente a
      putStrLn ("fib 0: " ++ show (fib 0))

    Exemplo completo:
    putStrLn $ "fib 0: " ++ show (fib 0)
    1. fib 0 calcula o valor: 0
    2. show (fib 0) converte para String: "0"
    3. "fib 0: " ++ "0" concatena: "fib 0: 0"
    4. putStrLn imprime: fib 0: 0
-}
