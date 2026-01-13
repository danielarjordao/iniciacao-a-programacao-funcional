{-# LANGUAGE NPlusKPatterns #-}
-- Esta extensão é necessária para usar padrões (n+k) como em fibAlt (n+2)
-- Padrões n+k são uma feature legada do Haskell, removida do Haskell 2010
-- Sem esta linha, o compilador daria erro: "Parse error in pattern: n + 2"

module Ficha2 where

-- Ficha Prática 2 - Programação Funcional

-- Tarefa 1: Pattern Matching e Ordem das Equações
-- A função f usa pattern matching para decidir qual equação usar.
-- O Haskell testa as equações DE CIMA PARA BAIXO e usa a PRIMEIRA que concordar.

f :: (Int, Char, Int) -> Int
f (y, 'a', x) = y + x       -- Se o 2º elemento for 'a': soma 1º + 3º
f (z, 'b', x) = z * x       -- Se o 2º elemento for 'b': multiplica 1º * 3º
f (x, y, z) = x             -- Para qualquer outro caso: retorna 1º elemento

{- Exemplos de avaliação:
   f (3,'a',5) = 3 + 5 = 8      -- usa 1ª equação
   f (9,'B',0) = 9              -- 'B' não é 'a' nem 'b', usa 3ª equação
   f (5,'b',4) = 5 * 4 = 20     -- usa 2ª equação

   A ordem importa! Se colocar f (x,y,z) = x primeiro,
   ela capturaria TODOS os casos (variáveis aceitam qualquer valor)
   e as outras equações nunca seriam executadas.
-}


-- Tarefa 2: Refatorar função usando PATTERN MATCHING

{- Versão ORIGINAL (do enunciado - difícil de ler!)
oppOriginal :: (Int, (Int, Int)) -> Int
oppOriginal z = if ((fst z) == 1)
                then (fst (snd z)) + (snd (snd z))
                else if ((fst z) == 2)
                     then (fst (snd z)) - (snd (snd z))
                     else 0

Problemas: muitos fst/snd aninhados, if-then-else confuso
-}

-- Versão MELHORADA: COM PATTERN MATCHING
opp :: (Int, (Int, Int)) -> Int
opp (1, (a, b)) = a + b      -- Se operação é 1: soma
opp (2, (a, b)) = a - b      -- Se operação é 2: subtrai
opp _           = 0          -- Qualquer outro caso: retorna 0

{- Vantagens do pattern matching:
   - Desconstrução direta da estrutura nos argumentos
   - Código muito mais limpo e legível
   - Elimina necessidade de fst/snd
   - Cada caso fica explícito e separado
-}


-- Tarefa 3: Definir funções usando GUARDAS

-- Parte 1: Nova versão de opp usando GUARDAS
oppGuardas :: (Int, (Int, Int)) -> Int
oppGuardas (op, (a, b))
    | op == 1   = a + b      -- Guarda: testa se op é 1
    | op == 2   = a - b      -- Guarda: testa se op é 2
    | otherwise = 0          -- Guarda: caso padrão (otherwise = True)

-- Parte 2: Factorial com proteção contra números negativos

{- Versão PERIGOSA (do enunciado - causa loop infinito com negativos!)
factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n - 1)

Problema: factorial (-1) entra em loop infinito!
  factorial (-1) = (-1) * factorial (-2)
  factorial (-2) = (-2) * factorial (-3)
  ... nunca chega ao caso base 0!
-}

-- Versão SEGURA: com guardas para proteger contra negativos
factorial :: Int -> Int
factorial n
    | n == 0    = 1                                  -- caso base
    | n > 0     = n * factorial (n - 1)             -- caso recursivo
    | otherwise = error "factorial: argumento negativo"


-- Tarefa 4: Números de Fibonacci

{- Definição matemática:
   fib(0) = 0
   fib(1) = 1
   fib(n) = fib(n-2) + fib(n-1) se n >= 2
-}

-- Tradução direta para Haskell usando pattern matching
fib :: Int -> Int
fib 0 = 0                       -- caso base 1
fib 1 = 1                       -- caso base 2
fib n = fib (n - 2) + fib (n - 1)   -- caso recursivo

-- Versão alternativa usando padrão (n+k)
-- Equivalente à função acima, mas usa (n+2) no pattern matching
fibAlt :: Int -> Int
fibAlt 0 = 0
fibAlt 1 = 1
fibAlt (n+2) = fibAlt (n+1) + fibAlt n

-- Sequência de Fibonacci: 0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55, ...

-- Tarefa 5: Definições Locais (let...in e where)

-- Parte 1: Analisar a função exemplo
-- Esta função demonstra o uso de let...in e where juntos
-- Nota: _ é uma variável anônima (usada para argumentos não utilizados)

exemplo :: Int -> (Int, Int)
exemplo y = let k = 100
                g (1,w,z) = w+z
                g (2,w,z) = w-z
                g (_,_,_) = k
            in (f y + f a + f b , g (y,k,c))
    where c = 10
          (a,b) = (3*c, f 2)
          f x = x + 7*c


-- Parte 2: Raízes reais de um polinômio ax² + bx + c

{- Calcula os valores de x que tornam ax² + bx + c = 0
   Usa fórmula de Bhaskara: x = (-b ± √(b² - 4ac)) / (2a)
   Retorna (r1, r2) onde r1 usa + e r2 usa -
-}

-- Versão ORIGINAL: usando where com guardas
raizes :: (Double,Double,Double) -> (Double,Double)
raizes (a,b,c) = (r1,r2)
    where r1 = (-b + r) / (2*a)
          r2 = (-b - r) / (2*a)
          d = b^2 - 4*a*c
          r | d >= 0 = sqrt d
            | d < 0  = error "raizes imaginarias"

-- Versão alternativa: usando let...in
raizesLet :: (Double,Double,Double) -> (Double,Double)
raizesLet (a,b,c) =
    let d = b^2 - 4*a*c
        r | d >= 0 = sqrt d
          | d < 0  = error "raizes imaginarias"
        r1 = (-b + r) / (2*a)
        r2 = (-b - r) / (2*a)
    in (r1,r2)

-- Versão sem guardas: calcula diretamente (assume d >= 0)
raizesSemGuardas :: (Double,Double,Double) -> (Double,Double)
raizesSemGuardas (a,b,c) = (r1, r2)
    where r1 = (-b + sqrt d) / (2*a)
          r2 = (-b - sqrt d) / (2*a)
          d = b^2 - 4*a*c

-- Versão compacta: tudo inline, sem definições locais
raizesCompacta :: (Double,Double,Double) -> (Double,Double)
raizesCompacta (a,b,c) =
    ( (-b + sqrt (b^2 - 4*a*c)) / (2*a),
      (-b - sqrt (b^2 - 4*a*c)) / (2*a) )
