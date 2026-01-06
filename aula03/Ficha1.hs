{-
  Módulo: Ficha1
  Descrição: Soluções das Tarefas 4 e 7 - Ficha Prática 1

  Este módulo contém funções que demonstram:
  - Pattern matching em tuplas
  - Uso de guards (guardas)
  - Cláusula where
  - Operações com strings e listas
  - Recursividade
  - Funções com listas
-}

module Ficha1 where

-- TAREFA 4

-- Exercício 1: Operações com Pares
-- Recebe dois pares de inteiros (a,b) e (c,d)
-- Retorna (a+c, b*d)
-- Tipo: (Int, Int) -> (Int, Int) -> (Int, Int)
-- Exemplo: somaEProduto (3, 5) (2, 4) = (5, 20)
somaEProduto :: (Int, Int) -> (Int, Int) -> (Int, Int)
somaEProduto (a, b) (c, d) = (a + c, b * d)

-- Exercício 2: Maior e Segundo Maior
-- Usa funções auxiliares max e min de forma aninhada
-- Tipo: Int -> Int -> Int -> (Int, Int)
-- Exemplo: maiorSegundoMaior 5 3 8 = (8, 5)
maiorSegundoMaior :: Int -> Int -> Int -> (Int, Int)
maiorSegundoMaior x y z = (max x (max y z), max (min x y) (max (min y z) (min x z)))
    -- Maior: max x (max y z) - compara x com o máximo entre y e z
    -- Segundo maior: max entre os três mínimos possíveis de pares
    -- Lógica: o segundo maior é o máximo entre:
    --   - min x y (menor entre x e y)
    --   - min y z (menor entre y e z)
    --   - min x z (menor entre x e z)
    -- Combinados: max (min x y) (max (min y z) (min x z))

-- Exercício 3: Ordenar Triplo Decrescente
-- Usa max e min para ordenar de forma direta
-- Tipo: (Int, Int, Int) -> (Int, Int, Int)
-- Exemplo: ordenaTriplo (5, 2, 8) = (8, 5, 2)
ordenaTriplo :: (Int, Int, Int) -> (Int, Int, Int)
ordenaTriplo (x, y, z) = (max x (max y z), x + y + z - max x (max y z) - min x (min y z), min x (min y z))
    -- Lógica:
    -- - maior: max x (max y z) - compara x com o máximo entre y e z
    -- - menor: min x (min y z) - compara x com o mínimo entre y e z
    -- - médio: soma total menos os extremos = o que sobra no meio (x + y + z - maior - menor)


-- Exercício 4: Validação de Triângulo
-- Recebe três lados de um possível triângulo
-- Retorna True se formam triângulo válido
-- Regra: soma de 2 lados > terceiro lado (para todos)
-- Tipo: Double -> Double -> Double -> Bool
-- Exemplo: trianguloValido 3 4 5 = True
trianguloValido :: Double -> Double -> Double -> Bool
trianguloValido a b c = (a + b > c) && (a + c > b) && (b + c > a)
    -- Desigualdade triangular:
    -- - a + b > c (soma de dois lados maior que o terceiro)
    -- - a + c > b (soma de dois lados maior que o terceiro)
    -- - b + c > a (soma de dois lados maior que o terceiro)
    -- Todas as três condições devem ser verdadeiras


-- Exercício 5: Abreviar Nome
-- Recebe nome completo de uma pessoa
-- Retorna primeiro nome + último sobrenome
-- Tipo: String -> String
-- Exemplo: abrev "Joao Carlos Martins Sarmento" = "Joao Sarmento"
-- Trata corretamente o caso de nome único (não repete)
abrev :: String -> String
abrev nome = if length palavras == 1 then head palavras else head palavras ++ " " ++ last palavras
    where palavras = words nome
    -- words: divide o texto em lista de palavras
    -- if-then-else: expressão condicional
    -- length: conta quantas palavras tem
    -- head: pega primeira palavra
    -- last: pega última palavra
    -- ++: concatena strings

-- Exemplos de uso (comentados)
{-
Teste no GHCi:

> :l Ficha1.hs
> somaEProduto (3, 5) (2, 4)
(5, 20)

> maiorSegundoMaior 5 3 8
(8, 5)

> ordenaTriplo (5, 2, 8)
(8, 5, 2)

> trianguloValido 3 4 5
True

> trianguloValido 1 2 5
False

> abrev "Joao Carlos Martins Sarmento"
"Joao Sarmento"

> abrev "Pedro"
"Pedro"
-}


-- TAREFA 7

-- Exercício 1: Exponenciação Inteira
-- Calcula x^y de forma recursiva (sem usar ^ ou **)
-- Tipo: Int -> Int -> Int
-- Exemplo: potencia 2 3 = 8 (2^3 = 2*2*2)
-- Exemplo: potencia 5 0 = 1 (qualquer número elevado a 0 é 1)
potencia :: Int -> Int -> Int
potencia _ 0 = 1                    -- caso base: x^0 = 1
potencia x y = x * potencia x (y-1) -- caso recursivo: x^y = x * x^(y-1)
    -- Exemplo: potencia 2 3
    -- = 2 * potencia 2 2
    -- = 2 * (2 * potencia 2 1)
    -- = 2 * (2 * (2 * potencia 2 0))
    -- = 2 * (2 * (2 * 1))
    -- = 2 * (2 * 2)
    -- = 2 * 4
    -- = 8

-- Exercício 2: Primeiro e Último de Lista
-- Retorna par com primeiro e último elemento
-- Tipo: [a] -> (a, a)
-- Exemplo: primeiroUltimo [1,2,3,4] = (1, 4)
-- CUIDADO: Falha com lista vazia!
primeiroUltimo :: [a] -> (a, a)
primeiroUltimo lista = (head lista, last lista)
    -- head: retorna primeiro elemento
    -- last: retorna último elemento
    -- Ambos falham com lista vazia []

-- Exercício 3: Lista com seu Comprimento
-- Retorna par com a lista e o número de elementos
-- Tipo: [a] -> ([a], Int)
-- Exemplo: listaComprimento [1,2,3] = ([1,2,3], 3)
listaComprimento :: [a] -> ([a], Int)
listaComprimento lista = (lista, length lista)
    -- length: retorna o número de elementos da lista
    -- Funciona com lista vazia: listaComprimento [] = ([], 0)

-- Exercício 4: Média de Lista de Números
-- Calcula a média aritmética de uma lista de números
-- Tipo: [Double] -> Double
-- Exemplo: media [2, 4, 6] = 4.0 (soma = 12, quantidade = 3)
-- CUIDADO: Falha com lista vazia (divisão por zero)!
media :: [Double] -> Double
media lista = sum lista / fromIntegral (length lista)
    -- sum: soma todos os elementos da lista
    -- length: conta quantos elementos tem
    -- fromIntegral: converte Int para Double (necessário para divisão)
    -- Por que fromIntegral?
    --   - length retorna Int
    --   - sum lista retorna Double
    --   - Precisamos converter Int -> Double para fazer a divisão
    -- Exemplo: media [2, 4, 6]
    -- = sum [2, 4, 6] / fromIntegral (length [2, 4, 6])
    -- = 12.0 / fromIntegral 3
    -- = 12.0 / 3.0
    -- = 4.0

