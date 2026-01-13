# Ficha Prática 2 - Programação Funcional

## Resumo

Nesta ficha pretende-se trabalhar sobre os seguintes conceitos básicos da linguagem de programação funcional Haskell:

- **Noção de padrão** e **concordância de padrões** (pattern matching)
- **Definições multi-clausais** de funções
- Relação entre definições multi-clausais e a **redução (cálculo) de expressões**
- Definição de funções com **guardas**
- **Definições locais** (`where` e `let...in`)
- Definição de **funções recursivas sobre listas**
- Definição de **tipos sinónimos** (type aliases)

## 1. Padrões (Patterns)

### O que é um Padrão?

Um **padrão** (ou *pattern*) é uma forma especial de escrever os argumentos de uma função que permite "desconstruir" valores estruturados e extrair seus componentes.

**Padrões válidos incluem:**

1. **Variáveis**: `x`, `y`, `lista`, `numero`
2. **Constantes**: `5`, `'A'`, `True`, `"texto"`
3. **Esquemas estruturais**: combinações de construtores com outros padrões

### Exemplos de Padrões Válidos

```haskell
-- Padrões simples
5                    -- constante inteira
'A'                  -- constante caractere
True                 -- constante booleana

-- Padrões em listas
[]                   -- lista vazia
[x]                  -- lista com exatamente um elemento
[x, y]               -- lista com exatamente dois elementos
[x, 'A', y]          -- lista com 3 elementos: qualquer, 'A', qualquer
(x:xs)               -- lista não vazia: cabeça x, cauda xs
(h:t)                -- mesma ideia: head h, tail t

-- Padrões em tuplas
(x, y)               -- par de dois elementos
(x, 8, (True, b))    -- tupla aninhada: (qualquer, 8, (True, qualquer))
(a, b, c)            -- tripla

-- Padrão wildcard (qualquer valor)
_                    -- ignora o valor (não cria variável)
```

### Restrições dos Padrões

**IMPORTANTE:** Padrões precisam ser **lineares** - não podem ter variáveis repetidas!

```haskell
-- ERRADO - variáveis repetidas
igual (x, x) = True       -- ERRO! x aparece duas vezes
igual (x, y) = False

-- CORRETO
igual (x, y) = x == y     -- usa comparação explícita

-- ERRADO - operações em padrões
soma (x + 1, y) = x + y   -- ERRO! (x + 1) não é um padrão válido

-- CORRETO
soma (x, y) = (x - 1) + y -- operação no corpo da função

-- ERRADO - valores calculados
dobro (2 * x, y) = x + y  -- ERRO! (2 * x) não é padrão

-- CORRETO - apenas construtores e constantes
dobro (x, y) = (2 * x) + y
```

### Por que Esses Não São Padrões?

Vamos analisar os exemplos do enunciado:

```haskell
-- [x, 'a', 1]
-- ERRO: mistura Char ('a') com Int (1) em uma lista
-- Listas devem ter todos elementos do MESMO tipo

-- (2, x, (z, x))
-- ERRO: variável x repetida (aparece duas vezes)
-- Padrões devem ser lineares

-- (4*5, y)
-- ERRO: (4*5) é uma expressão, não um padrão
-- Só pode usar constantes literais, não cálculos
```

### Exemplos de Funções com Padrões

```haskell
-- Padrão de constante
ehZero :: Int -> Bool
ehZero 0 = True          -- padrão: constante 0
ehZero _ = False         -- padrão: wildcard (qualquer outro)

-- Padrão de tupla
primeiro :: (a, b) -> a
primeiro (x, _) = x      -- padrão: (qualquer, ignorar)

segundo :: (a, b) -> b
segundo (_, y) = y       -- padrão: (ignorar, qualquer)

-- Padrão de lista
cabeca :: [a] -> a
cabeca (h:_) = h         -- padrão: (cabeça : resto ignorado)

cauda :: [a] -> [a]
cauda (_:t) = t          -- padrão: (ignorar : cauda)
```

## 2. Definições Multi-Clausais de Funções

### O que são Definições Multi-Clausais?

Uma função pode ser definida por **múltiplas equações** (cláusulas), cada uma com **padrões diferentes** nos argumentos. O Haskell escolhe qual equação usar através de **pattern matching**.

### Sintaxe

```haskell
nomeFuncao padrao1 = expressao1
nomeFuncao padrao2 = expressao2
nomeFuncao padrao3 = expressao3
...
```

### Exemplo 1: Função com Constantes

```haskell
f :: (Int, Char, Int) -> Int
f (y, 'a', x) = y + x       -- Se o char for 'a'
f (z, 'b', x) = z * x       -- Se o char for 'b'
f (x, y, z) = x             -- Qualquer outro caso
```

**Como funciona:**

```haskell
f (5, 'a', 3)
-- Testa 1ª equação: (5, 'a', 3) concorda com (y, 'a', x)?
--   y = 5, 'a' = 'a', x = 3 ok
-- Usa 1ª equação: y + x = 5 + 3 = 8
⇒ 8

f (4, 'b', 2)
-- Testa 1ª equação: (4, 'b', 2) concorda com (y, 'a', x)?
--   'b' ≠ 'a' - erro
-- Testa 2ª equação: (4, 'b', 2) concorda com (z, 'b', x)?
--   z = 4, 'b' = 'b', x = 2 ok
-- Usa 2ª equação: z * x = 4 * 2 = 8
⇒ 8

f (7, 'c', 1)
-- Testa 1ª equação: 'c' ≠ 'a' - erro
-- Testa 2ª equação: 'c' ≠ 'b' - erro
-- Testa 3ª equação: (7, 'c', 1) concorda com (x, y, z)?
--   x = 7, y = 'c', z = 1 ok
-- Usa 3ª equação: x = 7
⇒ 7
```

### Exemplo 2: Operador Lógico AND

```haskell
(&&) :: Bool -> Bool -> Bool
True  && True  = True
True  && False = False
False && True  = False
False && False = False

-- Versão mais eficiente (com wildcard):
(&&) :: Bool -> Bool -> Bool
True  && True = True
_     && _    = False    -- qualquer outro caso é False
```

### Exemplo 3: Função Fatorial

```haskell
fatorial :: Integer -> Integer
fatorial 0 = 1                    -- caso base
fatorial n = n * fatorial (n-1)   -- caso recursivo
```

**Avaliação:**
```haskell
fatorial 3
-- Testa 1ª equação: 3 ≠ 0 - erro
-- Testa 2ª equação: 3 concorda com n ok
-- n = 3
⇒ 3 * fatorial 2
⇒ 3 * (2 * fatorial 1)
⇒ 3 * (2 * (1 * fatorial 0))
-- Agora 0 concorda com primeira equação!
⇒ 3 * (2 * (1 * 1))
⇒ 6
```

### Ordem das Equações é IMPORTANTE!

As equações são testadas **de cima para baixo**. A primeira que concordar é usada.

```haskell
-- ERRADO - ordem ruim!
descreveNumMal :: Int -> String
descreveNumMal n = "algum número"    -- captura TUDO primeiro!
descreveNumMal 0 = "zero"            -- NUNCA será alcançado
descreveNumMal 1 = "um"              -- NUNCA será alcançado

-- CORRETO - específico antes de genérico
descreveNumBom :: Int -> String
descreveNumBom 0 = "zero"
descreveNumBom 1 = "um"
descreveNumBom n = "algum número"    -- captura o resto
```

**Teste:**
```haskell
descreveNumMal 0
-- Testa 1ª equação: 0 concorda com n? ok (n pode ser qualquer Int)
-- Usa 1ª equação!
⇒ "algum número"    -- Errado!

descreveNumBom 0
-- Testa 1ª equação: 0 = 0? ok
⇒ "zero"            -- Correto!
```

### Pattern Matching em Listas

```haskell
-- Descrever tamanho da lista
tamanho :: [a] -> String
tamanho []     = "vazia"              -- padrão: lista vazia
tamanho [x]    = "um elemento"        -- padrão: lista com 1 elemento
tamanho [x,y]  = "dois elementos"     -- padrão: lista com 2 elementos
tamanho _      = "três ou mais"       -- padrão: qualquer outra lista

-- Primeiro elemento (head)
primeiro :: [a] -> a
primeiro (x:_) = x                    -- padrão: x é a cabeça
primeiro []    = error "lista vazia"  -- padrão: lista vazia

-- Segundo elemento
segundo :: [a] -> a
segundo (_:x:_) = x                   -- padrão: ignora 1º, pega 2º
segundo _       = error "lista muito curta"
```

**Avaliação:**
```haskell
tamanho []
-- Testa 1ª equação: [] = []? ok
⇒ "vazia"

tamanho [5]
-- Testa 1ª equação: [5] = []? - erro
-- Testa 2ª equação: [5] = [x]? ok (x = 5)
⇒ "um elemento"

primeiro [10, 20, 30]
-- [10, 20, 30] é o mesmo que (10 : 20 : 30 : [])
-- Testa 1ª equação: (10:20:30:[]) concorda com (x:_)?
--   x = 10, _ = [20, 30] ok
⇒ 10
```

## 3. Inferência de Tipos em Funções Multi-Clausais

### Tipo Declarado vs Tipo Inferido

Quando definimos uma função, **não é obrigatório** declarar seu tipo. O Haskell **infere** (deduz) o tipo automaticamente.

```haskell
-- Sem declaração de tipo
seg (x, y) = y

-- O que o Haskell infere?
-- seg recebe uma tupla (?, ?)
-- Retorna o segundo elemento
-- Não há restrições sobre os tipos
-- Tipo mais geral possível:
seg :: (a, b) -> b
```

### Podemos Especializar o Tipo

Se declararmos um tipo **mais específico**, o Haskell aceita:

```haskell
-- Tipo específico (mais restritivo)
seg :: (Bool, Int) -> Int
seg (x, y) = y

-- Agora seg só aceita tuplas (Bool, Int)
seg (True, 5)    --  OK
seg (False, 10)  --  OK
seg (True, 'a')  --  ERRO: 'a' não é Int
seg (5, 10)      --  ERRO: 5 não é Bool
```

### Tipo Inferido é o Mais Geral

Sem declaração, o Haskell escolhe o tipo **mais geral** (polimórfico):

```haskell
-- Função identidade
id x = x
-- Tipo inferido: id :: a -> a
-- Funciona com QUALQUER tipo!

-- Função que retorna primeiro elemento
fst (x, y) = x
-- Tipo inferido: fst :: (a, b) -> a
-- Funciona com QUALQUER tupla!

-- Função que duplica em lista
duplica x = [x, x]
-- Tipo inferido: duplica :: a -> [a]
```

### Todas as Equações Devem Ter o Mesmo Tipo

```haskell
--  CORRETO - todas as equações retornam Int
f :: (Int, Char, Int) -> Int
f (y, 'a', x) = y + x    -- retorna Int
f (z, 'b', x) = z * x    -- retorna Int
f (x, y, z) = x          -- retorna Int

-- ERRADO - tipos diferentes!
g (x, 'a') = x           -- retorna o tipo de x (genérico)
g (x, _)   = "erro"      -- retorna String
-- ERRO: não pode retornar ora 'a', ora String
```

## 4. Listas: Construtores e Padrões

### Os Dois Construtores de Listas

Toda lista em Haskell é construída usando apenas **dois construtores primitivos**:

1. **`[]`** - A lista vazia
   ```haskell
   [] :: [a]
   ```

2. **`(:)`** - O operador "cons" (construtor)
   ```haskell
   (:) :: a -> [a] -> [a]
   ```

   Dados um elemento `x` e uma lista `xs`, `x:xs` cria uma nova lista com `x` na frente.

### Notação de Listas

```haskell
-- Notação abreviada (açúcar sintático)
[1, 2, 3]

-- Notação completa com construtores
1 : 2 : 3 : []

-- Equivalente (: é associativa à direita)
1 : (2 : (3 : []))

-- Outras formas equivalentes
1 : [2, 3]
1 : 2 : [3]
```

**Teste no ghci:**
```haskell
> [1, 2, 3] == 1:2:3:[]
True

> [1, 2, 3] == 1:[2, 3]
True

> [1, 2, 3] == 1:2:[3]
True
```

### Anatomia de uma Lista

```haskell
[1, 2, 3, 4]
 │  └─────┘
 │     └─── cauda (tail): [2, 3, 4]
 └───────── cabeça (head): 1

-- Usando (:)
    1 : [2, 3, 4]
    │       │
  head    tail

-- Completamente expandido
1 : 2 : 3 : 4 : []
│   │   │   │   └─ lista vazia
│   │   │   └────── 4 : []
│   │   └────────── 3 : (4 : [])
│   └────────────── 2 : (3 : (4 : []))
└────────────────── 1 : (2 : (3 : (4 : [])))
```

### Padrões Sobre Listas

Podemos usar os construtores `[]` e `(:)` para criar padrões:

```haskell
-- Padrão: lista vazia
[]

-- Padrão: lista não vazia (cabeça : cauda)
(x:xs)      -- x é o 1º elemento, xs é o resto
(h:t)       -- h é head, t é tail
(a:b:c:ds)  -- a, b, c são os 3 primeiros, ds é o resto

-- Padrão: lista com exatamente 1 elemento
[x]         -- equivalente a (x:[])

-- Padrão: lista com exatamente 2 elementos
[x, y]      -- equivalente a (x:y:[]) ou (x:(y:[]))

-- Padrão: lista com pelo menos 2 elementos
(x:y:resto) -- x e y são os 2 primeiros, resto é o que sobra
```

**IMPORTANTE:** Padrões com `(:)` precisam de **parênteses**!

```haskell
-- ERRADO - sem parênteses
cabeca x:xs = x        -- ERRO de sintaxe!

--  CORRETO - com parênteses
cabeca (x:xs) = x      --  OK
```

### Exemplos de Funções com Padrões de Lista

```haskell
-- Testar se lista é vazia
vazia :: [a] -> Bool
vazia []     = True
vazia (x:xs) = False

-- Obter primeiro elemento (head)
cabeca :: [a] -> a
cabeca []     = error "lista vazia"
cabeca (x:xs) = x

-- Obter cauda (tail)
cauda :: [a] -> [a]
cauda []     = error "lista vazia"
cauda (x:xs) = xs

-- Obter segundo elemento
segundo :: [a] -> a
segundo []       = error "lista vazia"
segundo [x]      = error "lista muito curta"
segundo (x:y:_)  = y

-- Primeiros dois elementos
primeiros2 :: [a] -> [a]
primeiros2 []       = []
primeiros2 [x]      = [x]
primeiros2 (x:y:_)  = [x, y]
```

**Avaliação passo a passo:**
```haskell
vazia []
-- Testa 1ª equação: [] = []? ok
⇒ True

vazia [1, 2, 3]
-- [1,2,3] é o mesmo que (1:2:3:[])
-- Testa 1ª equação: (1:2:3:[]) = []? - erro
-- Testa 2ª equação: (1:2:3:[]) concorda com (x:xs)?
--   x = 1, xs = [2, 3] ok
⇒ False

segundo [10, 20, 30]
-- [10,20,30] é (10:20:30:[])
-- Testa 1ª equação: (10:20:30:[]) = []? - erro
-- Testa 2ª equação: (10:20:30:[]) = [x]? - erro
-- Testa 3ª equação: (10:20:30:[]) concorda com (x:y:_)?
--   x = 10, y = 20, _ = [30] ok
⇒ y
⇒ 20
```

## 5. Funções Recursivas sobre Listas

### Estratégia de Definição

Como listas são definidas recursivamente:
- Caso base: `[]` (lista vazia)
- Caso recursivo: `(x:xs)` (elemento + lista menor)

**Estratégia para funções sobre listas:**

1. **Caso base**: Definir o que fazer com `[]`
2. **Caso recursivo**: Definir o que fazer com `(x:xs)`
   - Processar `x` (a cabeça)
   - Chamar recursivamente a função com `xs` (a cauda)
   - Combinar os resultados

### Exemplo 1: Soma dos Elementos

```haskell
soma :: [Int] -> Int
soma []     = 0                -- lista vazia: soma é 0
soma (h:t)  = h + soma t       -- h + soma do resto
```

**Avaliação passo a passo:**
```haskell
soma [1, 2, 3]
⇒ soma (1:2:3:[])
-- Testa 1ª equação: (1:2:3:[]) = []? - erro
-- Testa 2ª equação: (1:2:3:[]) = (h:t)?
--   h = 1, t = [2, 3] ok
⇒ 1 + soma [2, 3]
⇒ 1 + soma (2:3:[])
⇒ 1 + (2 + soma [3])
⇒ 1 + (2 + soma (3:[]))
⇒ 1 + (2 + (3 + soma []))
-- Agora [] concorda com 1ª equação!
⇒ 1 + (2 + (3 + 0))
⇒ 1 + (2 + 3)
⇒ 1 + 5
⇒ 6
```

### Exemplo 2: Comprimento da Lista

```haskell
comprimento :: [a] -> Int
comprimento []     = 0                 -- lista vazia: tamanho 0
comprimento (_:t)  = 1 + comprimento t -- 1 + tamanho do resto
```

Note o uso de `_` (wildcard) - não precisamos do valor de `x`, apenas contamos!

**Avaliação:**
```haskell
comprimento [10, 20, 30, 40]
⇒ 1 + comprimento [20, 30, 40]
⇒ 1 + (1 + comprimento [30, 40])
⇒ 1 + (1 + (1 + comprimento [40]))
⇒ 1 + (1 + (1 + (1 + comprimento [])))
⇒ 1 + (1 + (1 + (1 + 0)))
⇒ 4
```

### Exemplo 3: Distâncias de Pontos à Origem

```haskell
distancias :: [(Float, Float)] -> [Float]
distancias []           = []
distancias ((x,y):xys)  = sqrt (x^2 + y^2) : distancias xys
```

**Como funciona:**
- Caso base: lista de pontos vazia → lista de distâncias vazia
- Caso recursivo:
  - Calcular distância do primeiro ponto `(x, y)` à origem
  - Calcular recursivamente as distâncias dos pontos restantes
  - Juntar tudo com `:`

**Avaliação:**
```haskell
distancias [(3, 4), (0, 5)]
-- Testa 2ª equação: ((3,4) : [(0,5)]) = ((x,y):xys)?
--   x = 3, y = 4, xys = [(0, 5)] ok
⇒ sqrt (3^2 + 4^2) : distancias [(0, 5)]
⇒ sqrt (9 + 16) : distancias [(0, 5)]
⇒ sqrt 25 : distancias [(0, 5)]
⇒ 5.0 : distancias [(0, 5)]

-- Próxima recursão: distancias [(0, 5)]
⇒ 5.0 : (sqrt (0^2 + 5^2) : distancias [])
⇒ 5.0 : (sqrt 25 : distancias [])
⇒ 5.0 : (5.0 : distancias [])

-- Caso base: distancias []
⇒ 5.0 : (5.0 : [])
⇒ 5.0 : [5.0]
⇒ [5.0, 5.0]
```

### Exemplo 4: Reverter uma Lista

```haskell
reverso :: [a] -> [a]
reverso []     = []
reverso (h:t)  = reverso t ++ [h]
```

**Avaliação:**
```haskell
reverso [1, 2, 3]
⇒ reverso [2, 3] ++ [1]
⇒ (reverso [3] ++ [2]) ++ [1]
⇒ ((reverso [] ++ [3]) ++ [2]) ++ [1]
⇒ (([] ++ [3]) ++ [2]) ++ [1]
⇒ ([3] ++ [2]) ++ [1]
⇒ [3, 2] ++ [1]
⇒ [3, 2, 1]
```

### Exemplo 5: Máximo de uma Lista

```haskell
maximo :: [Int] -> Int
maximo []      = error "lista vazia"
maximo [x]     = x                    -- lista com 1 elemento
maximo (h:t)   = max h (maximo t)     -- max entre h e o máximo do resto
```

**Avaliação:**
```haskell
maximo [3, 7, 2, 9, 1]
⇒ max 3 (maximo [7, 2, 9, 1])
⇒ max 3 (max 7 (maximo [2, 9, 1]))
⇒ max 3 (max 7 (max 2 (maximo [9, 1])))
⇒ max 3 (max 7 (max 2 (max 9 (maximo [1]))))
⇒ max 3 (max 7 (max 2 (max 9 1)))
⇒ max 3 (max 7 (max 2 9))
⇒ max 3 (max 7 9)
⇒ max 3 9
⇒ 9
```

### Exemplo 6: Filtrar Elementos Pares

```haskell
pares :: [Int] -> [Int]
pares []     = []
pares (x:xs) = if even x
               then x : pares xs    -- inclui x na lista resultado
               else pares xs        -- não inclui x
```

**Avaliação:**
```haskell
pares [1, 2, 3, 4, 5, 6]
⇒ pares [2, 3, 4, 5, 6]              -- 1 é ímpar, não inclui
⇒ 2 : pares [3, 4, 5, 6]             -- 2 é par, inclui
⇒ 2 : pares [4, 5, 6]                -- 3 é ímpar, não inclui
⇒ 2 : 4 : pares [5, 6]               -- 4 é par, inclui
⇒ 2 : 4 : pares [6]                  -- 5 é ímpar, não inclui
⇒ 2 : 4 : 6 : pares []               -- 6 é par, inclui
⇒ 2 : 4 : 6 : []
⇒ [2, 4, 6]
```

### Padrão Geral de Recursão em Listas

```haskell
funcao :: [a] -> resultado
funcao []     = valorBase           -- O que retornar para lista vazia?
funcao (x:xs) = combinar x (funcao xs)  -- Como combinar x com o resultado do resto?
```

**Exemplos do padrão:**

| Função | Valor Base | Combinar |
|--------|------------|----------|
| `soma` | `0` | `x + (soma xs)` |
| `produto` | `1` | `x * (produto xs)` |
| `length` | `0` | `1 + (length xs)` |
| `reverso` | `[]` | `(reverso xs) ++ [x]` |
| `concat` | `[]` | `x ++ (concat xs)` |


## 6. Tipos Sinónimos (Type Aliases)

### O que são Tipos Sinónimos?

**Tipos sinónimos** (ou *type aliases*) permitem criar **nomes alternativos** para tipos existentes. Não criam novos tipos, apenas dão nomes mais descritivos para tipos complexos.

### Sintaxe

```haskell
type NomeNovo = TipoExistente
```

### Exemplo Básico: String

O tipo `String` em Haskell é um **sinónimo** pré-definido:

```haskell
type String = [Char]
```

Isso significa que `String` e `[Char]` são **exatamente a mesma coisa**!

```haskell
mensagem :: String
mensagem = "Olá"

-- É equivalente a:
mensagem :: [Char]
mensagem = ['O', 'l', 'á']

-- São o mesmo tipo!
> "Olá" == ['O', 'l', 'á']
True
```

### Vantagens dos Tipos Sinónimos

1. **Legibilidade**: Código mais claro e autodescritivo
2. **Documentação**: O nome do tipo explica seu propósito
3. **Manutenibilidade**: Mudar a representação em um só lugar
4. **Abstração**: Esconder detalhes de implementação

### Exemplo Completo: Lista Telefônica

Imagine que queremos representar uma lista telefônica. Cada entrada tem:
- Nome (String)
- Número de telefone (String)
- Endereço de email (String)

**Sem tipos sinónimos (confuso):**

```haskell
-- Tipo confuso - muitos Strings!
adicionaContato :: (String, String, String) -> [(String, String, String)] -> [(String, String, String)]
adicionaContato entrada lista = entrada : lista

-- Difícil saber o que é cada String!
```

**Com tipos sinónimos (claro):**

```haskell
-- Definir sinónimos
type Nome = String
type Telefone = String
type Email = String
type Entrada = (Nome, Telefone, Email)
type LTelef = [Entrada]

-- Agora a função fica muito mais clara!
adicionaContato :: Entrada -> LTelef -> LTelef
adicionaContato entrada lista = entrada : lista
```

### Exemplo: Funções sobre Lista Telefônica

```haskell
-- Definições de tipos
type Nome = String
type Telefone = String
type Email = String
type Entrada = (Nome, Telefone, Email)
type LTelef = [Entrada]

-- Obter todos os emails
emails :: LTelef -> [Email]
emails []              = []
emails ((_,_,em):t)    = em : emails t

-- Obter todos os nomes
nomes :: LTelef -> [Nome]
nomes []           = []
nomes ((n,_,_):t)  = n : nomes t

-- Buscar telefone por nome
buscaTelefone :: Nome -> LTelef -> Maybe Telefone
buscaTelefone _ []  = Nothing
buscaTelefone nome ((n,tel,_):resto)
    | nome == n  = Just tel
    | otherwise  = buscaTelefone nome resto

-- Exemplo de uso:
agenda :: LTelef
agenda = [ ("João", "123456", "joao@email.com")
         , ("Maria", "789012", "maria@email.com")
         , ("Pedro", "345678", "pedro@email.com")
         ]
```

**Avaliação:**
```haskell
emails agenda
⇒ emails [("João","123456","joao@email.com"), ("Maria","789012","maria@email.com"), ("Pedro","345678","pedro@email.com")]
-- Primeira entrada: (n,tel,em) = ("João","123456","joao@email.com")
⇒ "joao@email.com" : emails [("Maria","789012","maria@email.com"), ("Pedro","345678","pedro@email.com")]
⇒ "joao@email.com" : "maria@email.com" : emails [("Pedro","345678","pedro@email.com")]
⇒ "joao@email.com" : "maria@email.com" : "pedro@email.com" : emails []
⇒ "joao@email.com" : "maria@email.com" : "pedro@email.com" : []
⇒ ["joao@email.com", "maria@email.com", "pedro@email.com"]
```

### Tipo Equivalente Expandido

Lembre-se que `String` também é um sinónimo para `[Char]`. Então:

```haskell
type Entrada = (String, String, String)
-- É o mesmo que:
type Entrada = ([Char], [Char], [Char])

type LTelef = [Entrada]
-- É o mesmo que:
type LTelef = [(String, String, String)]
-- Que é o mesmo que:
type LTelef = [([Char], [Char], [Char])]
```

Portanto, a assinatura:
```haskell
emails :: LTelef -> [String]
```

É equivalente a:
```haskell
emails :: [([Char], [Char], [Char])] -> [[Char]]
```

Percebe como os sinónimos tornam o código **muito mais legível**?

### Mais Exemplos de Tipos Sinónimos

```haskell
-- Representar coordenadas 2D
type Coordenada = (Float, Float)
type Ponto = Coordenada  -- sinónimo de sinónimo!

distancia :: Ponto -> Ponto -> Float
distancia (x1, y1) (x2, y2) = sqrt ((x2-x1)^2 + (y2-y1)^2)

-- Representar matriz como lista de listas
type Matriz = [[Int]]

-- Representar um dicionário
type Chave = String
type Valor = Int
type Dicionario = [(Chave, Valor)]

-- Representar grafo
type Vertice = Int
type Aresta = (Vertice, Vertice)
type Grafo = [Aresta]
```

### Tipos Sinónimos com Parâmetros

Tipos sinónimos podem ser **polimórficos** (ter variáveis de tipo):

```haskell
-- Par ordenado genérico
type Par a = (a, a)

duplica :: a -> Par a
duplica x = (x, x)

-- Dicionário genérico
type Dicionario chave valor = [(chave, valor)]

busca :: Eq k => k -> Dicionario k v -> Maybe v
busca _ []            = Nothing
busca k ((k',v):resto)
    | k == k'   = Just v
    | otherwise = busca k resto
```

### Diferença: `type` vs `data` vs `newtype`

| Palavra-chave | Propósito | Cria novo tipo? |
|---------------|-----------|-----------------|
| `type` | Sinónimo (alias) |  Não - apenas outro nome |
| `data` | Novo tipo algébrico |  Sim - tipo completamente novo |
| `newtype` | Wrapper eficiente |  Sim - mas representação igual |

**Exemplo:**
```haskell
-- type: apenas apelido
type Email = String
-- Email e String são EXATAMENTE o mesmo tipo

-- data: tipo novo
data Cor = Vermelho | Verde | Azul
-- Cor é um tipo DIFERENTE de qualquer outro

-- newtype: tipo novo mas eficiente
newtype Idade = MkIdade Int
-- Idade é diferente de Int, mas mesma representação em runtime
```

## 7. Revisão: `where` e `let...in` (Definições Locais)

### Cláusula `where`

Já vimos `where` na aula anterior, mas vamos revisar com mais exemplos de listas:

```haskell
-- Ordenar lista de 3 elementos
ordena3 :: [Int] -> [Int]
ordena3 [a, b, c] = [minimo, medio, maximo]
    where
        minimo = min a (min b c)
        maximo = max a (max b c)
        medio = a + b + c - minimo - maximo

-- Estatísticas de uma lista
estatisticas :: [Int] -> (Int, Int, Float)
estatisticas xs = (minimo, maximo, media)
    where
        minimo = minimum xs
        maximo = maximum xs
        media = fromIntegral (sum xs) / fromIntegral (length xs)
```

### Expressão `let...in`

```haskell
-- Calcular distância entre dois pontos
distanciaPontos :: (Float, Float) -> (Float, Float) -> Float
distanciaPontos (x1, y1) (x2, y2) =
    let dx = x2 - x1
        dy = y2 - y1
    in sqrt (dx^2 + dy^2)

-- Processar lista em etapas
processaLista :: [Int] -> [Int]
processaLista xs =
    let semNegativos = filter (>= 0) xs
        dobrados = map (*2) semNegativos
    in reverse dobrados
```

### Definições Locais de Funções

Podemos definir **funções locais** em `where` ou `let...in`:

```haskell
-- Função auxiliar em where
somaQuadrados :: [Int] -> Int
somaQuadrados xs = sum (map quadrado xs)
    where
        quadrado x = x * x  -- função local!

-- Função auxiliar em let...in
contaPares :: [Int] -> Int
contaPares xs =
    let ehPar x = even x
    in length (filter ehPar xs)

-- Múltiplas funções auxiliares
processaTexto :: String -> String
processaTexto texto = mapeiaCaracteres texto
    where
        mapeiaCaracteres = map transformaChar
        transformaChar c
            | ehVogal c = toUpper c
            | otherwise = c
        ehVogal c = c `elem` "aeiouAEIOU"
```

## Resumo da Ficha 2

### Conceitos Principais

| Conceito | Descrição | Exemplo |
|----------|-----------|---------|
| **Padrões** | Desconstruir valores estruturados | `(x:xs)`, `(a,b)`, `[]` |
| **Pattern Matching** | Escolher equação por concordância | `f 0 = 1; f n = n * f (n-1)` |
| **Definições Multi-Clausais** | Múltiplas equações para uma função | Várias linhas com padrões diferentes |
| **Listas** | Construídas com `[]` e `(:)` | `[1,2,3]` = `1:2:3:[]` |
| **Recursão em Listas** | Caso base `[]` + caso `(x:xs)` | `soma [] = 0; soma (h:t) = h + soma t` |
| **Tipos Sinónimos** | Nomes alternativos para tipos | `type String = [Char]` |
| **Definições Locais** | `where` e `let...in` | Variáveis/funções locais |

### Padrão Recursivo para Listas

```haskell
-- Estrutura geral
minhaFuncao :: [a] -> Resultado
minhaFuncao []     = casoBase
minhaFuncao (x:xs) = combina x (minhaFuncao xs)
```

### Ordem de Prioridade em Pattern Matching

1. Padrões mais **específicos** primeiro (constantes, estruturas fixas)
2. Padrões mais **genéricos** depois (variáveis, wildcard)
3. Último: padrão "catch-all" (`_` ou variável genérica)

### Boas Práticas

 **Fazer:**
- Usar padrões para desconstruir valores
- Colocar casos específicos antes de casos gerais
- Usar `_` quando não precisar do valor
- Usar tipos sinónimos para clareza
- Definir funções auxiliares localmente quando apropriado

 **Evitar:**
- Variáveis repetidas em padrões
- Operações ou cálculos em padrões
- Padrão genérico antes de específico
- Tipos confusos sem sinónimos descritivos
