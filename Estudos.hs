-- ============================================
-- EXEMPLOS BÁSICOS DE HASKELL
-- ============================================

-- 1. FUNÇÕES SIMPLES
-- Funções em Haskell são como funções matemáticas
-- Não precisam de parênteses nos parâmetros

dobro :: Int -> Int          -- Tipo: recebe Int, retorna Int
dobro x = x * 2

quadrado :: Int -> Int
quadrado x = x * x

-- Função com dois parâmetros
soma :: Int -> Int -> Int    -- Tipo: recebe Int, recebe Int, retorna Int
soma a b = a + b

-- Exemplo de uso (valores imutáveis)
x = soma 3 5                 -- x sempre será 8
y = dobro 10                 -- y sempre será 20


-- 2. TRABALHANDO COM LISTAS
-- Listas são estruturas fundamentais em Haskell

numeros :: [Int]
numeros = [1, 2, 3, 4, 5]

-- Pattern matching: separa lista em cabeça (primeiro) e cauda (resto)
-- [] = lista vazia
-- (x:xs) = x é o primeiro elemento, xs é o resto

somaLista :: [Int] -> Int
somaLista [] = 0                    -- Caso base: lista vazia = 0
somaLista (x:xs) = x + somaLista xs -- Caso recursivo: primeiro + soma do resto

-- Outra função recursiva: tamanho da lista
tamanho :: [Int] -> Int
tamanho [] = 0
tamanho (x:xs) = 1 + tamanho xs


-- 3. FUNÇÕES DE ALTA ORDEM
-- Funções que recebem outras funções como parâmetro

-- map: aplica uma função em cada elemento
-- map dobro [1,2,3] resulta em [2,4,6]

-- filter: mantém apenas elementos que passam no teste
-- filter (> 5) [3,6,2,8] resulta em [6,8]

-- Exemplo combinando as duas
listaTransformada :: [Int]
listaTransformada = map quadrado (filter (> 3) numeros)
-- Pega numeros > 3: [4,5]
-- Aplica quadrado: [16,25]


-- 4. GUARDAS (alternativa ao if/else)
-- Similar ao switch/case, mas mais expressivo

classifica :: Int -> String
classifica n
    | n < 0     = "Negativo"
    | n == 0    = "Zero"
    | n > 0     = "Positivo"
    | otherwise = "Impossível"  -- otherwise é como "default"


-- 5. FUNÇÕES LAMBDA (funções anônimas)
-- Úteis para passar funções rápidas sem definir nome

-- Sintaxe: \parametro -> expressão
triplo = \x -> x * 3

-- Uso comum com map/filter
-- map (\x -> x + 1) [1,2,3] resulta em [2,3,4]


-- 6. LISTAS POR COMPREENSÃO
-- Sintaxe elegante para criar listas

quadrados :: [Int]
quadrados = [x * x | x <- [1..10]]  -- Quadrados de 1 a 10

pares :: [Int]
pares = [x | x <- [1..20], even x]  -- Apenas números pares


-- ============================================
-- PROGRAMA PRINCIPAL (IO)
-- ============================================
-- IO é necessário para entrada/saída
-- O bloco 'do' permite sequenciar ações

main :: IO ()
main = do
    putStrLn "=== EXEMPLOS HASKELL ==="

    putStrLn "\n1. Funções simples:"
    putStrLn ("   dobro 5 = " ++ show (dobro 5))
    putStrLn ("   quadrado 7 = " ++ show (quadrado 7))
    putStrLn ("   soma 10 20 = " ++ show (soma 10 20))

    putStrLn "\n2. Listas e recursão:"
    putStrLn ("   numeros = " ++ show numeros)
    putStrLn ("   somaLista numeros = " ++ show (somaLista numeros))
    putStrLn ("   tamanho numeros = " ++ show (tamanho numeros))

    putStrLn "\n3. Funções de alta ordem:"
    putStrLn ("   map dobro [1,2,3,4] = " ++ show (map dobro [1,2,3,4]))
    putStrLn ("   filter (> 5) [3,6,2,8] = " ++ show (filter (> 5) [3,6,2,8]))
    putStrLn ("   listaTransformada = " ++ show listaTransformada)

    putStrLn "\n4. Guardas:"
    putStrLn ("   classifica (-5) = " ++ classifica (-5))
    putStrLn ("   classifica 0 = " ++ classifica 0)
    putStrLn ("   classifica 10 = " ++ classifica 10)

    putStrLn "\n5. Lambda:"
    putStrLn ("   triplo 4 = " ++ show (triplo 4))
    putStrLn ("   map (\\x -> x + 1) [1,2,3] = " ++ show (map (\x -> x + 1) [1,2,3]))

    putStrLn "\n6. Compreensão de listas:"
    putStrLn ("   quadrados (1 a 10) = " ++ show quadrados)
    putStrLn ("   pares (1 a 20) = " ++ show pares)
