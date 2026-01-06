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

### Expressões Condicionais: `if-then-else`

Em Haskell, `if-then-else` é uma **expressão** (não um comando como em outras linguagens). Isso significa que sempre retorna um valor e pode ser usada em qualquer lugar onde uma expressão é esperada.

#### Sintaxe Básica

```haskell
if condicao then expressao1 else expressao2
```

**Características importantes:**
- `condicao` deve ser do tipo `Bool` (True ou False)
- `expressao1` e `expressao2` devem ter o **mesmo tipo**
- O `else` é **obrigatório** (não existe `if` sem `else`)
- Retorna `expressao1` se `condicao` for `True`, senão retorna `expressao2`

#### Exemplo 1: Valor Absoluto

```haskell
absoluto :: Int -> Int
absoluto x = if x >= 0 then x else (-x)
```

**Avaliação:**
```haskell
absoluto 5    ⇒ if 5 >= 0 then 5 else (-5)  ⇒ 5
absoluto (-3) ⇒ if (-3) >= 0 then (-3) else 3  ⇒ 3
```

#### Exemplo 2: Sinal de um Número

```haskell
sinal :: Int -> String
sinal x = if x > 0 then "positivo" else "não positivo"
```

#### Exemplo 3: Dentro de Expressões

Como `if-then-else` é uma expressão, pode ser usado dentro de outras expressões:

```haskell
-- Calcular desconto
precoFinal :: Float -> Float
precoFinal preco = preco * if preco > 100 then 0.9 else 1.0

-- Em operações aritméticas
maximo :: Int -> Int -> Int
maximo a b = if a > b then a else b

resultado :: Int -> Int -> Int
resultado x y = (if x > 0 then x else 0) + (if y > 0 then y else 0)
```

#### `if-then-else` Aninhado (else if)

Para testar múltiplas condições, você pode aninhar `if-then-else`:

```haskell
classificaNota :: Int -> String
classificaNota nota =
    if nota >= 9 then "Excelente"
    else if nota >= 7 then "Bom"
    else if nota >= 5 then "Suficiente"
    else "Insuficiente"
```

**Avaliação passo a passo:**
```haskell
classificaNota 8
⇒ if 8 >= 9 then "Excelente"
  else if 8 >= 7 then "Bom"
  else if 8 >= 5 then "Suficiente"
  else "Insuficiente"
⇒ if False then "Excelente"
  else if 8 >= 7 then "Bom" ...
⇒ if 8 >= 7 then "Bom"
  else if 8 >= 5 then "Suficiente"
  else "Insuficiente"
⇒ if True then "Bom" ...
⇒ "Bom"
```

**Outro exemplo - calculadora simples:**
```haskell
calcula :: Char -> Int -> Int -> Int
calcula op a b =
    if op == '+' then a + b
    else if op == '-' then a - b
    else if op == '*' then a * b
    else if op == '/' then a `div` b
    else 0  -- operador inválido
```

### Guardas (Guards): Condições com `|` e `otherwise`

**Guards** (guardas) são uma forma elegante de escrever funções com múltiplas condições. Usam o símbolo `|` (pipe) seguido de uma expressão booleana.

#### Sintaxe Básica

```haskell
nomeFuncao argumentos
    | condicao1 = resultado1
    | condicao2 = resultado2
    | condicao3 = resultado3
    | otherwise = resultadoPadrao
```

**Como funciona:**
- Cada `|` marca uma "guarda" (condição)
- Haskell testa as condições **de cima para baixo**
- Retorna o resultado da **primeira** condição verdadeira
- `otherwise` é equivalente a `True` - usado como caso padrão (sempre verdadeiro)

#### Exemplo 1: Classificação de Nota

```haskell
classificaNota :: Int -> String
classificaNota nota
    | nota >= 9 = "Excelente"
    | nota >= 7 = "Bom"
    | nota >= 5 = "Suficiente"
    | otherwise = "Insuficiente"
```

**Avaliação passo a passo:**
```haskell
classificaNota 8
-- Testa: 8 >= 9? Não
-- Testa: 8 >= 7? Sim! Retorna "Bom"
⇒ "Bom"

classificaNota 4
-- Testa: 4 >= 9? Não
-- Testa: 4 >= 7? Não
-- Testa: 4 >= 5? Não
-- otherwise? Sim (sempre True)
⇒ "Insuficiente"
```

#### O que é `otherwise`?

`otherwise` é simplesmente um **sinônimo** para `True` definido no Prelude:

```haskell
otherwise :: Bool
otherwise = True
```

É usado por convenção como a **última guarda**, garantindo que sempre haverá uma alternativa final. Poderia ser substituído por `True`, mas `otherwise` é mais legível.

```haskell
-- Essas duas funções são equivalentes
f1 x | x > 0     = "positivo"
     | otherwise = "não positivo"

f2 x | x > 0 = "positivo"
     | True  = "não positivo"  -- funciona, mas otherwise é mais claro
```

#### Exemplo 2: Sinal de um Número

```haskell
sinal :: Int -> String
sinal n
    | n > 0     = "positivo"
    | n < 0     = "negativo"
    | otherwise = "zero"
```

#### Exemplo 3: IMC (Índice de Massa Corporal)

```haskell
imc :: Float -> Float -> String
imc peso altura
    | resultado < 18.5 = "Abaixo do peso"
    | resultado < 25.0 = "Peso normal"
    | resultado < 30.0 = "Sobrepeso"
    | otherwise        = "Obesidade"
    where resultado = peso / (altura ^ 2)
```

Aqui combinamos **guards** com **where** - muito comum em Haskell!

#### Exemplo 4: Máximo de Três Números

```haskell
max3 :: Int -> Int -> Int -> Int
max3 a b c
    | a >= b && a >= c = a
    | b >= c           = b
    | otherwise        = c
```

#### Guards vs `if-then-else` vs `if-then-else if`

Vamos comparar as três formas de escrever condicionais em Haskell:

**Forma 1: `if-then-else` simples (2 condições)**

```haskell
absoluto1 :: Int -> Int
absoluto1 x = if x >= 0 then x else (-x)
```

- Bom para: **2 alternativas simples**
- Conciso e direto
- Limitação: Só funciona bem com duas opções

**Forma 2: `if-then-else if-then-else` aninhado (múltiplas condições)**

```haskell
classificaNota1 :: Int -> String
classificaNota1 nota =
    if nota >= 9 then "Excelente"
    else if nota >= 7 then "Bom"
    else if nota >= 5 then "Suficiente"
    else "Insuficiente"
```

- Funciona para múltiplas condições
- Desvantagem: Verboso - muito `else if`
- Desvantagem: Menos legível com muitas condições
- Desvantagem: Indentação pode ficar confusa

**Forma 3: Guards com `|` e `otherwise` (múltiplas condições)**

```haskell
classificaNota2 :: Int -> String
classificaNota2 nota
    | nota >= 9 = "Excelente"
    | nota >= 7 = "Bom"
    | nota >= 5 = "Suficiente"
    | otherwise = "Insuficiente"
```

- **Mais limpo** para múltiplas condições
- Mais legível - cada condição em sua linha
- Alinhamento vertical facilita leitura
- `otherwise` deixa claro o caso padrão
- Idiomático em Haskell (forma preferida)

#### Comparação Lado a Lado

Vamos implementar a mesma função das três formas:

```haskell
-- Tarifa de taxi baseada na distância

-- Forma 1: if-then-else simples (só 2 opções - limitado!)
tarifa1 :: Float -> Float
tarifa1 km = if km <= 5 then 10.0 else 10.0 + (km - 5) * 2.0

-- Forma 2: if-then-else aninhado
tarifa2 :: Float -> Float
tarifa2 km =
    if km <= 5 then 10.0
    else if km <= 15 then 10.0 + (km - 5) * 2.0
    else if km <= 30 then 30.0 + (km - 15) * 1.5
    else 52.5 + (km - 30) * 1.0

-- Forma 3: Guards (MAIS LIMPO!)
tarifa3 :: Float -> Float
tarifa3 km
    | km <= 5   = 10.0
    | km <= 15  = 10.0 + (km - 5) * 2.0
    | km <= 30  = 30.0 + (km - 15) * 1.5
    | otherwise = 52.5 + (km - 30) * 1.0
```

#### Quando Usar Cada Forma?

| Situação | Usar |
|----------|------|
| **2 alternativas simples** | `if-then-else` |
| **Expressão dentro de outra expressão** | `if-then-else` |
| **3+ condições booleanas** | **Guards** (`\|` e `otherwise`) |
| **Lógica clara e legível** | **Guards** |
| **Pattern matching estrutural** | `case...of` |

#### Guards com Expressões Complexas

Você pode usar qualquer expressão booleana nas guards:

```haskell
-- Pode usar funções, operadores, etc.
descreveNum :: Int -> String
descreveNum n
    | even n && n > 0  = "par positivo"
    | odd n && n > 0   = "ímpar positivo"
    | n == 0           = "zero"
    | otherwise        = "negativo"

-- Pode chamar funções auxiliares
podeVotar :: Int -> String
podeVotar idade
    | ehMenor idade    = "Não pode votar"
    | ehIdoso idade    = "Voto facultativo"
    | otherwise        = "Voto obrigatório"
    where
        ehMenor i = i < 16
        ehIdoso i = i >= 70
```

#### Importante: Ordem das Guards

A **ordem importa**! Haskell testa de cima para baixo e retorna na **primeira** condição verdadeira.

```haskell
-- ERRADO - ordem ruim!
classificaMal :: Int -> String
classificaMal nota
    | nota >= 0 = "Pelo menos zero"  -- sempre pega primeiro!
    | nota >= 5 = "Suficiente"       -- nunca alcançado
    | nota >= 7 = "Bom"              -- nunca alcançado
    | otherwise = "Negativo"

-- CORRETO - do mais específico ao mais geral
classificaBem :: Int -> String
classificaBem nota
    | nota >= 9 = "Excelente"
    | nota >= 7 = "Bom"
    | nota >= 5 = "Suficiente"
    | nota >= 0 = "Insuficiente"
    | otherwise = "Nota inválida"
```

#### Resumo: Guards

| Característica | Guards |
|----------------|--------|
| Sintaxe | `\| condicao = resultado` |
| Caso padrão | `otherwise` (equivalente a `True`) |
| Ordem | Testa de cima para baixo |
| Uso ideal | 3+ condições booleanas |
| Legibilidade | Excelente para múltiplas condições |
| Combinável com | `where`, `let...in`, pattern matching |

#### Quando Usar `if-then-else`?

Use **`if-then-else`** quando:
- Tem apenas **duas** alternativas simples
- Precisa de uma expressão condicional **dentro** de outra expressão
- A condição e os resultados são curtos e diretos

**Exemplos apropriados:**
```haskell
max2 :: Int -> Int -> Int
max2 a b = if a > b then a else b

parOuImpar :: Int -> String
parOuImpar n = if even n then "par" else "ímpar"

ajustaValor :: Int -> Int
ajustaValor x = (if x < 0 then 0 else x) + 10
```

Use **guards** quando:
- Tem **múltiplas** condições (3 ou mais)
- As condições são complexas
- Quer código mais legível para casos múltiplos

Use **`case...of`** quando:
- Precisa fazer pattern matching estrutural
- Trabalha com tipos de dados algébricos

#### `if-then-else` com `let...in`

Você pode combinar `if-then-else` com `let...in`:

```haskell
-- Calcular segundo maior de três números
segundoMaior :: Int -> Int -> Int -> Int
segundoMaior x y z =
    let m = max x (max y z)
        s = if m == x then max y z
            else if m == y then max x z
            else max x y
    in s
```

#### Erros Comuns

**ERRO: Esquecer o `else`**
```haskell
-- ERRADO - não compila!
teste x = if x > 0 then "positivo"

-- CORRETO
teste x = if x > 0 then "positivo" else "não positivo"
```

**ERRO: Tipos diferentes no `then` e `else`**
```haskell
-- ERRADO - tipos incompatíveis!
teste x = if x > 0 then "positivo" else 0

-- CORRETO - mesmo tipo
teste x = if x > 0 then "positivo" else "zero ou negativo"
```

**ERRO: Usar `if` como statement (comando)**
```haskell
-- ERRADO - if é expressão, não comando
teste x =
    if x > 0
        print "positivo"  -- ERRO!

-- Em Haskell, use do-notation para IO (veremos mais tarde)
```

#### Resumo

| Característica | `if-then-else` |
|----------------|----------------|
| Tipo | Expressão |
| Condição | Deve ser `Bool` |
| `else` | Obrigatório |
| Tipos | `then` e `else` devem ter mesmo tipo |
| Uso | Simples alternativas, dentro de expressões |
| Alternativa | Guards (para múltiplas condições) |

### Definindo Valores Locais: `where` e `let...in`

Em Haskell, existem duas formas principais de definir valores locais (variáveis auxiliares) dentro de uma função: usando **`where`** ou **`let...in`**.

#### Cláusula `where`

A cláusula `where` permite definir valores auxiliares **após** a expressão principal da função. É muito útil para tornar o código mais legível, evitando repetições.

**Sintaxe:**
```haskell
nomefuncao argumentos = expressao
    where
        var1 = valor1
        var2 = valor2
        ...
```

**Exemplo:**
```haskell
-- Calcular área de um círculo
areaCirculo :: Float -> Float
areaCirculo r = pi * r2
    where
        r2 = r * r
        pi = 3.14159
```

**Exemplo - ordenar três números:**
```haskell
ord3 :: (Int, Int, Int) -> (Int, Int, Int)
ord3 (a, b, c) = (m1, m2, m3)
    where
        m1 = max a (max b c)  -- maior
        m3 = min a (min b c)  -- menor
        m2 = a + b + c - m1 - m3  -- médio
```

**Vantagens do `where`:**
- As variáveis auxiliares aparecem depois da expressão principal
- Útil quando a lógica principal é simples e as definições auxiliares são mais complexas
- Permite definir múltiplas variáveis de forma clara
- O escopo das variáveis abrange toda a definição da função (incluindo guards)

#### Expressão `let...in`

A expressão `let...in` permite definir valores auxiliares **antes** de usá-los na expressão final. É uma **expressão** (não apenas uma cláusula), então pode ser usada em qualquer lugar onde uma expressão é esperada.

**Sintaxe:**
```haskell
let var1 = valor1
    var2 = valor2
    ...
in expressao
```

**Exemplo:**
```haskell
-- Calcular área de um cilindro
areaCilindro :: Float -> Float -> Float
areaCilindro r h =
    let areaBase = pi * r * r
        areaLateral = 2 * pi * r * h
    in 2 * areaBase + areaLateral
```

**Exemplo - maior e segundo maior de três números:**
```haskell
maior2 :: Int -> Int -> Int -> (Int, Int)
maior2 x y z =
    let m = max x (max y z)
        s = if m == x then max y z
            else if m == y then max x z
            else max x y
    in (m, s)
```

**Exemplo - `let` dentro de uma expressão:**
```haskell
-- Calcular hipotenusa
hipotenusa :: Float -> Float -> Float
hipotenusa a b = sqrt (let a2 = a * a
                           b2 = b * b
                       in a2 + b2)
```

**Vantagens do `let...in`:**
- As definições aparecem antes do seu uso (ordem "top-down")
- Pode ser usado dentro de outras expressões (guards, condicionais, etc.)
- Útil quando você quer "construir" o resultado passo a passo
- Permite definições muito localizadas

#### Comparação: `where` vs `let...in`

| Característica | `where` | `let...in` |
|----------------|---------|------------|
| Posição | Depois da expressão | Antes da expressão |
| Tipo | Cláusula sintática | Expressão |
| Escopo | Toda a função (incluindo guards) | Apenas a expressão do `in` |
| Uso | No final da definição da função | Em qualquer lugar |
| Legibilidade | Bom quando a expressão principal é simples | Bom quando há construção passo a passo |

**Exemplo equivalente - mesma função com `where` e `let...in`:**

```haskell
-- Com where
distancia1 :: (Float, Float) -> (Float, Float) -> Float
distancia1 (x1, y1) (x2, y2) = sqrt (dx2 + dy2)
    where
        dx = x2 - x1
        dy = y2 - y1
        dx2 = dx * dx
        dy2 = dy * dy

-- Com let...in
distancia2 :: (Float, Float) -> (Float, Float) -> Float
distancia2 (x1, y1) (x2, y2) =
    let dx = x2 - x1
        dy = y2 - y1
        dx2 = dx * dx
        dy2 = dy * dy
    in sqrt (dx2 + dy2)
```

**Quando usar cada um?**

Use **`where`** quando:
- A expressão principal é simples e auto-explicativa
- As definições auxiliares são "suporte" para entender a função
- Você quer que as variáveis estejam disponíveis em múltiplos guards

Use **`let...in`** quando:
- Você quer construir o resultado passo a passo
- Precisa de definições locais dentro de expressões (guards, if-then-else)
- Prefere ordem "top-down" (definir antes de usar)
- Quer definições muito localizadas e específicas

**Nota importante:** Ambas as formas são equivalentes em poder expressivo. A escolha é principalmente uma questão de estilo e legibilidade.

### Pattern Matching com `case...of`

A expressão `case...of` permite fazer **pattern matching** (correspondência de padrões) dentro de uma expressão. É similar ao uso de múltiplas equações de função, mas pode ser usado em qualquer lugar dentro do código.

#### Sintaxe Básica

```haskell
case expressao of
    padrao1 -> resultado1
    padrao2 -> resultado2
    ...
    padraoN -> resultadoN
```

O Haskell avalia `expressao` e depois tenta fazer correspondência com cada padrão, de cima para baixo. Quando encontra um padrão compatível, retorna o resultado correspondente.

#### Exemplo 1: Classificar Nota

```haskell
-- Classificar nota numérica
classificaNota :: Int -> String
classificaNota n = case n of
    10 -> "Excelente"
    9  -> "Muito Bom"
    8  -> "Bom"
    7  -> "Suficiente"
    _  -> "Insuficiente"  -- _ captura qualquer valor
```

#### Exemplo 2: Pattern Matching em Listas

```haskell
-- Descrever tamanho da lista
descreveLista :: [a] -> String
descreveLista xs = case xs of
    []      -> "Lista vazia"
    [x]     -> "Lista com um elemento"
    [x, y]  -> "Lista com dois elementos"
    _       -> "Lista com vários elementos"
```

#### Exemplo 3: Pattern Matching em Tuplas

```haskell
-- Calcular operação baseada em tupla
calculaOp :: (Char, Int, Int) -> Int
calculaOp t = case t of
    ('+', a, b) -> a + b
    ('-', a, b) -> a - b
    ('*', a, b) -> a * b
    ('/', a, b) -> a `div` b
    _           -> 0  -- operação desconhecida
```

#### Exemplo 4: Abreviar Nome (usando `case`)

```haskell
-- Versão com case...of
abrev :: String -> String
abrev nome =
    let palavras = words nome
    in case palavras of
        []  -> ""
        [x] -> x
        _   -> head palavras ++ " " ++ last palavras
```

Este exemplo mostra como `case` pode ser útil quando você precisa fazer pattern matching em uma expressão calculada (neste caso, `words nome`).

#### Exemplo 5: `case` Aninhado

```haskell
-- Descrever par de valores
descreverPar :: (Int, Int) -> String
descreverPar p = case p of
    (0, 0) -> "Origem"
    (0, y) -> case compare y 0 of
        GT -> "Eixo Y positivo"
        LT -> "Eixo Y negativo"
        EQ -> "Origem"  -- já tratado acima, mas por completude
    (x, 0) -> case compare x 0 of
        GT -> "Eixo X positivo"
        LT -> "Eixo X negativo"
        EQ -> "Origem"
    (x, y) -> "Ponto no plano"
```

#### Comparação: Múltiplas Equações vs `case...of`

Estas duas definições são equivalentes:

```haskell
-- Com múltiplas equações (pattern matching direto)
tamanho :: [a] -> String
tamanho []  = "vazia"
tamanho [x] = "um elemento"
tamanho _   = "vários elementos"

-- Com case...of
tamanho' :: [a] -> String
tamanho' lista = case lista of
    []  -> "vazia"
    [x] -> "um elemento"
    _   -> "vários elementos"
```

#### Quando Usar `case...of`?

Use **`case...of`** quando:
- Precisa fazer pattern matching **dentro** de uma expressão (não na definição da função)
- Quer fazer pattern matching em uma sub-expressão calculada
- Tem pattern matching em conjunto com `let...in` ou `where`
- Quer agrupar lógica de pattern matching em um único lugar

Use **múltiplas equações** quando:
- O pattern matching é na definição principal da função
- Quer código mais limpo e direto
- Cada caso pode ter lógica complexa independente

#### Guards vs `case...of`

**Guards** testam condições booleanas; **`case`** faz correspondência de padrões estruturais.

```haskell
-- Com guards (condições booleanas)
classificaIdade :: Int -> String
classificaIdade idade
    | idade < 0  = "Inválido"
    | idade < 13 = "Criança"
    | idade < 18 = "Adolescente"
    | otherwise  = "Adulto"

-- Com case (padrões estruturais)
primeiraCor :: [String] -> String
primeiraCor cores = case cores of
    []      -> "sem cor"
    (c:_)   -> c
```

**Você pode combinar as duas abordagens:**

```haskell
processaLista :: [Int] -> String
processaLista xs = case xs of
    []     -> "vazia"
    (x:_)  | x > 0     -> "começa positivo"
           | x < 0     -> "começa negativo"
           | otherwise -> "começa com zero"
```

#### Padrão `_` (wildcard)

O underscore `_` é um padrão especial que corresponde a **qualquer valor** mas não o vincula a uma variável. Use quando não precisa do valor.

```haskell
-- Ignorar segundo elemento da tupla
primeiro :: (a, b) -> a
primeiro par = case par of
    (x, _) -> x  -- não usamos o segundo elemento
```

#### Resumo

| Característica | `case...of` |
|----------------|-------------|
| Tipo | Expressão |
| Uso | Pattern matching em qualquer lugar |
| Quando usar | Dentro de expressões, com valores calculados |
| Equivalente | Múltiplas equações de função |
| Combinável com | Guards, `let...in`, `where` |

**Exemplo completo combinando tudo:**

```haskell
analisaLista :: [Int] -> String
analisaLista xs =
    let tamanho = length xs
        soma = sum xs
    in case xs of
        [] -> "Lista vazia"
        [x] -> "Um único elemento: " ++ show x
        _  | soma > 100  -> "Soma grande: " ++ show soma
           | tamanho > 10 -> "Lista longa: " ++ show tamanho ++ " elementos"
           | otherwise    -> "Lista normal"
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
