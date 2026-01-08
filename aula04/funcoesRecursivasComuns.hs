{-
    Testar o uso das seguintes funções recursivas comuns em Haskell:
    - take
    - tail
    - zipWith
-}

-- take: Retorna os primeiros n elementos de uma listaInt
primeirosDois :: [a] -> [a] -- Função genérica que funciona para qualquer tipo de lista
primeirosDois = take 2  -- Retorna os primeiros 2 elementos da lista

primeirosTres :: [a] -> [a] -- Função genérica que funciona para qualquer tipo de lista
primeirosTres = take 3  -- Retorna os primeiros 3 elementos da lista

-- tail: Retorna a listaInt sem o primeiro elemento
listaSemPrimeiro :: [a] -> [a]
listaSemPrimeiro = tail  -- Retorna a lista sem o primeiro elemento

-- zipWith: Combina duas listaInts aplicando uma função aos elementos correspondentes
somaListas :: [Int] -> [Int] -> [Int]
somaListas = zipWith (+)  -- Soma os elementos correspondentes de duas listas

produtoListas :: [Int] -> [Int] -> [Int]
produtoListas = zipWith (*)  -- Multiplica os elementos correspondentes de duas listas


listaInt :: [Int]
listaInt = [1, 2, 3, 4, 5, 6]

listaChar :: [Char]
listaChar = ['a', 'b', 'c', 'd', 'e']

listaIntA :: [Int]
listaIntA = [1, 2, 3]

listaIntB :: [Int]
listaIntB = [4, 5, 6, 7]

main :: IO ()
main = do
    putStrLn "Testes das funções recursivas comuns."
    putStrLn "Primeiros dois elementos da listaInt [1,2,3,4,5,6]:"
    print (primeirosDois listaInt)        -- Deve retornar [1, 2]
    putStrLn "Primeiros três elementos da listaChar ['a','b','c','d','e']:"
    print (primeirosTres listaChar)       -- Deve retornar "abc"
    putStrLn "ListaInt sem o primeiro elemento [1,2,3,4,5,6]:"
    print (listaSemPrimeiro listaInt)     -- Deve retornar [2, 3, 4, 5, 6]
    putStrLn "Soma e produto de duas listasInt [1,2,3] e [4,5,6,7]:"
    print (somaListas listaIntA listaIntB)      -- Deve retornar [5, 7, 9]
    putStrLn "Soma de duas listasInt [4,5,6,7] e [1,2,3]:"
    print (somaListas listaIntB listaIntA)      -- Deve retornar [5, 7, 9]
    putStrLn "Produto de duas listasInt [1,2,3] e [4,5,6,7]:"
    print (produtoListas listaIntA listaIntB)  -- Deve retornar [4, 10, 18]
