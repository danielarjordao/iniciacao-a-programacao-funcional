# Ficha Prática 1 - Programação Funcional

## Valores, Expressões e Tipos

### Valores

Valores (ou constantes) são as entidades básicas da linguagem Haskell. São dados que não precisam ser calculados. Valores são casos especiais de expressões.

**Exemplos:**
```haskell
5
67.9
True
'u'
"abcd"
```

### Expressões

Expressões são obtidas combinando valores com funções, operadores (que também são funções) e variáveis.

**Exemplos:**
```haskell
3.8 + 4.6
True && (not False)
((*) 4 ((+) 9 3))  -- operadores em notação prefixa
8 * 5 + 4 * ((2 + 3) * 8 - 6)
(toLower (toUpper 'x'))
```

### Tipos

Um conceito muito importante: toda expressão tem um **tipo**. Os tipos servem para classificar os dados de acordo com suas características.

Em Haskell, escrevemos `e :: T` para dizer que a expressão `e` é do tipo `T` (ou "e tem tipo T").

**Exemplos:**
```haskell
5 :: Int
67.9 :: Float
True :: Bool
'u' :: Char
(3.8 + 4.6) :: Float
(True && (not False)) :: Bool
((*) 4 ((+) 9 3)) :: Int
(8 * 5 + 4 * ((2 + 3) * 8 - 6)) :: Int
(toLower (toUpper 'x')) :: Char
```

### Tipos Básicos

O Haskell oferece os seguintes tipos básicos:

- `Bool`: Booleanos - `True`, `False`
- `Char`: Caracteres - `'a'`, `'x'`, `'R'`, `'7'`, `'\n'`, ...
- `Int`: Inteiros de tamanho limitado - `1`, `-4`, `23467`, ...
- `Integer`: Inteiros de tamanho ilimitado - `-6`, `36`, `45763456623443249`, ...
- `Float`: Números de vírgula flutuante - `3.5`, `-45.78`, ...
- `Double`: Números de vírgula flutuante de dupla precisão - `-45.63`, `3.678`, `51.2E7`, ...
- `()`: Unit - `()`

### Tipos Compostos

#### Produtos Cartesianos (Tuplas)

```haskell
(a1, a2, ..., an) :: (T1, T2, ..., Tn)
```

Sendo `a1` do tipo `T1`, `a2` do tipo `T2`, ... `an` do tipo `Tn`.

**Exemplos:**
```haskell
(3, 'd') :: (Int, Char)
(True, 5.7, 3) :: (Bool, Float, Int)
('k', (6, 2), False) :: (Char, (Int, Int), Bool)
```

#### Listas

```haskell
[a1, a2, ..., an] :: [T]
```

Todos os elementos `ai` da lista são do tipo `T`.

**Exemplos:**
```haskell
[3, 4, 3, 7, 8, 2, 5] :: [Int]
['r', 'c', 'e', '4', 'd'] :: [Char]  -- equivalente a "rce4d"
[('a',5), ('d', 3), ('h', 9)] :: [(Char, Int)]
[[5,6], [3], [9,2,6], [], [1,4]] :: [[Int]]
```

**Nota:** Uma lista de caracteres `['r', 'c', 'e', '4', 'd']` é equivalente à string `"rce4d"`.

### Cálculo do Valor de uma Expressão

O interpretador de Haskell (`ghci`) usa as definições de funções e operadores como regras de cálculo para calcular o valor de uma expressão. Ele faz isso passo a passo (chamamos de passos de redução).

**Exemplo:**
```haskell
8 * 5 + 4 * ((2 + 3) * 8 - 6)
⇒ 40 + 4 * (5 * 8 - 6)
⇒ 40 + 4 * (40 - 6)
⇒ 40 + 4 * 34
⇒ 40 + 136
⇒ 176
```

## Funções: Tipos e Definição

Em Haskell, as funções são tratadas da mesma forma que valores - elas também têm tipos.

### Tipo de Funções

```haskell
f :: T1 -> T2
```

Funções que recebem valores do tipo `T1` e devolvem valores do tipo `T2`.

A aplicação da função `f` ao argumento `a` do tipo `T1` escreve-se `(f a)` e tem tipo `T2`.

**Exemplos de funções pré-definidas:**
```haskell
toLower :: Char -> Char
not :: Bool -> Bool
ord :: Char -> Int
chr :: Int -> Char
fst :: (a, b) -> a
tail :: [a] -> [a]
```

### Funções Polimórficas

Existem funções que podem trabalhar com mais de um tipo (chamamos de funções polimórficas). O Haskell usa **variáveis de tipo** (`a`, `b`, `c`, ...) para representar "qualquer tipo".

**Exemplos:**
- `fst :: (a, b) -> a` - pode ser usada com tuplas de quaisquer tipos
- `tail :: [a] -> [a]` - pode ser usada com listas de qualquer tipo

Quando você usa essas funções, as variáveis de tipo são substituídas automaticamente pelos tipos concretos que você está usando.

### Compatibilidade de Tipos

Um princípio fundamental: o tipo de uma função precisa ser compatível com os tipos dos seus argumentos. O Haskell verifica isso antes de executar o código, o que evita muitos erros.

**Exemplo de erro:**
```haskell
> tail 45
-- ERRO: tail espera uma lista, mas recebeu um Int
```

### Funções Pré-definidas

O Haskell oferece um conjunto de funções pré-definidas (no módulo `Prelude`):

- **Operadores lógicos**: `&&`, `||`, `not`
- **Operadores relacionais**: `>`, `<=`, `==`
- **Operadores sobre produtos cartesianos**: `fst`, `snd`
- **Operadores sobre listas**: `head`, `tail`, `length`, `reverse`, `concat`, `++`

### Funções Definidas pelo Programador

Podemos definir novas funções. Uma função é definida por uma equação que relaciona os seus argumentos com o resultado pretendido:

```haskell
<nomefuncao> <arg1> ... <argn> = <expressao>
```

**Exemplos:**
```haskell
ex a = 50 * a

funcao1 x y = x + (70 * y)
```

Depois de definidas, estas funções podem ser utilizadas para construir novas expressões, que serão avaliadas de acordo com as definições das funções.

**Exemplo de avaliação:**
```haskell
funcao1 (ex 10) 1
⇒ (ex 10) + (70 * 1)
⇒ (50 * 10) + (70 * 1)
⇒ 500 + (70 * 1)
⇒ 500 + 70
⇒ 570
```

### Declaração de Tipos

O Haskell consegue descobrir (inferir) o tipo de cada função automaticamente. Porém, é considerado **boa prática** incluir explicitamente na definição de uma função o seu tipo.

**Exemplo - função que retorna o maior de dois inteiros:**
```haskell
maior :: (Int, Int) -> Int
maior (x, y) = if (x > y) then x else y
```

**Exemplo - função que retorna o maior de três inteiros:**
```haskell
maiorde3 :: Int -> Int -> Int -> Int
maiorde3 x y z = (maior ((maior (x, y)), z))
```

## Módulos de Código Haskell

### O que é um Módulo?

As definições de funções não podem ser digitadas diretamente no interpretador de Haskell. Elas precisam ser escritas em um arquivo - chamamos isso de **módulo**.

Um módulo é um arquivo contendo um conjunto de definições (de tipos, funções, classes, etc.) que são lidas pelo interpretador.

### Estrutura de um Módulo

Um módulo Haskell é guardado num arquivo com extensão `.hs`. O nome do arquivo deve corresponder ao nome do módulo, que precisa ser declarado na primeira linha.

**Exemplo - arquivo `Teste.hs`:**
```haskell
module Teste where

funcao1 x y = x + (70 * y)

ex a = 50 * a
```

## Importação de Módulos

### Como Importar Módulos

Um programa em Haskell é formado por um conjunto de módulos. As definições de cada módulo podem ser usadas internamente ou exportadas para outros módulos.

Para usar definições de outro módulo, é necessário **importá-lo explicitamente** usando a declaração `import`.

**Exemplo - importar o módulo `Data.Char`:**
```haskell
module Conv1 where

import Data.Char

con = toLower 'A'

fun x = toUpper x
```

### Módulo Prelude

Uma exceção importante: o módulo `Prelude` contém as definições básicas da linguagem Haskell e está **sempre disponível** automaticamente em todos os módulos, sem precisar importá-lo.

## Introdução às Funções Recursivas

### O que é Recursividade?

Uma função é **recursiva** quando ela aparece na própria expressão que a define. Dizemos que a função chama a si própria (ou se invoca a si própria).

O cálculo da função termina porque sempre atingimos o **caso de paragem** (ou caso base).

### Exemplo - Função Fatorial

```haskell
-- Função que calcula o fatorial de um número
fatorial :: Integer -> Integer
fatorial 0 = 1
fatorial n = n * fatorial (n - 1)
```

Neste exemplo:
- **Caso de paragem**: `fatorial 0 = 1`
- **Caso recursivo**: `fatorial n = n * fatorial (n - 1)`

**Exemplo de avaliação:**
```haskell
fatorial 3
⇒ 3 * fatorial 2
⇒ 3 * (2 * fatorial 1)
⇒ 3 * (2 * (1 * fatorial 0))
⇒ 3 * (2 * (1 * 1))
⇒ 3 * (2 * 1)
⇒ 3 * 2
⇒ 6
```

### Características das Funções Recursivas

Toda função recursiva bem definida possui:

- **Caso base (ou caso de paragem)**: condição que termina a recursão
- **Caso recursivo**: a função chama a si mesma com argumentos diferentes
- **Convergência**: os argumentos devem se aproximar do caso base a cada chamada
