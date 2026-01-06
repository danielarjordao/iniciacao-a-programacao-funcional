# Resolução - Ficha Prática 1

## Tarefa 1 - Inferência de Tipos e Polimorfismo

### Objetivo
Observar o mecanismo de inferência de tipos do Haskell e o polimorfismo das funções `fst` e `tail`.

### Comandos e Resultados

#### 1. Ativar exibição de tipos
```haskell
> :set +t
```
**O que faz:** Ativa a exibição automática dos tipos de cada resultado.

> **Explicação do comando:**
>
> - `:set` = comando para configurar opções do GHCi
> - `+t` = flag que ativa a exibição de tipos
> - Após este comando, cada resultado mostrará seu tipo automaticamente
> - Para desativar: `:unset +t`
> - É como ligar um "modo de aprendizado" que mostra os tipos!

#### 2. Função `fst` com tupla (Int, Char)
```haskell
> fst (4, 'a')
```
**Resultado:** `4`
**Tipo:** `it :: Num a => a` (ou `Integer`)

**Aprendizado:** `fst` retorna o primeiro elemento da tupla.

> **Explicando o tipo `Num a => a`:**
>
> - `it` = é o nome da variável que guarda o último resultado no GHCi
> - `::` = "tem tipo"
> - `Num a =>` = **restrição de tipo** (type constraint)
>   - Significa: "`a` precisa ser um tipo numérico"
>   - `Num` é uma **classe de tipos** que inclui Int, Integer, Float, Double, etc.
> - `a` = uma **variável de tipo** que representa qualquer tipo numérico
>
> **Em português:** "it tem tipo 'a', onde 'a' é qualquer tipo que seja numérico"
>
> Por que não é simplesmente `Int`? Porque o Haskell não sabe ainda se você quer:
> - `Int` (inteiro limitado)
> - `Integer` (inteiro ilimitado)
> - `Float` (decimal)
> - `Double` (decimal dupla precisão)
>
> O número `4` pode ser qualquer um desses tipos! O Haskell deixa a escolha em aberto.

#### 3. Função `snd` com tupla (Int, Char)
```haskell
> snd (4, 'a')
```
**Resultado:** `'a'`
**Tipo:** `it :: Char`

**Aprendizado:** `snd` retorna o segundo elemento da tupla.

> **Explicação detalhada:**
>
> - `snd` vem de "second" (segundo)
> - Tipo da função: `snd :: (a, b) -> b`
>   - Recebe uma tupla `(a, b)`
>   - Retorna o segundo elemento (tipo `b`)
> - Na tupla `(4, 'a')`:
>   - `a = Int` (o 4)
>   - `b = Char` (o 'a')
>   - Retorna tipo `Char`
> - `it :: Char` = "o resultado é do tipo Char"
>   - Não tem restrição `=>` porque Char é um tipo concreto, não genérico

#### 4. Função `fst` com tupla (Double, Int)
```haskell
> fst (5.6, 3)
```
**Resultado:** `5.6`
**Tipo:** `it :: Fractional a => a` (ou `Double`)

**Aprendizado:** `fst` funciona com qualquer tipo de tupla - é **polimórfica**.

> **Explicando o tipo `Fractional a => a`:**
>
> - `Fractional a =>` = restrição de tipo
>   - Significa: "`a` precisa ser um tipo de número decimal"
>   - `Fractional` inclui Float e Double
>   - É mais restritivo que `Num` (nem todo número é decimal!)
>
> Por que `Fractional` e não `Num`? Porque `5.6` tem ponto decimal!
> - `4` poderia ser Int, Integer, Float, Double → `Num a => a`
> - `5.6` só pode ser Float ou Double → `Fractional a => a`

#### 5. Informações sobre `fst`
```haskell
> :i fst
```
**Resultado:** Mostra informações completas sobre a função (módulo, tipo, etc.)

> **O que `:i` mostra:**
> ```
> fst :: (a, b) -> a  -- Defined in 'GHC.Tuple'
> ```
> - Tipo da função
> - Módulo onde foi definida
> - Se for uma classe de tipos, mostra as instâncias
> - `:i` = ":info" (informação)

```haskell
> :t fst
```
**Resultado:** `fst :: (a, b) -> a`

> **Explicação do tipo `(a, b) -> a`:**
>
> - `(a, b)` = entrada: uma tupla com dois elementos
>   - `a` = tipo do primeiro elemento (qualquer tipo)
>   - `b` = tipo do segundo elemento (qualquer tipo, pode ser diferente de `a`)
> - `->` = retorna
> - `a` = saída: o primeiro elemento da tupla
>
> **Exemplos de uso:**
> - `fst (5, 'x')` → `a=Int, b=Char` → retorna `Int`
> - `fst (True, 3.5)` → `a=Bool, b=Double` → retorna `Bool`
> - `fst ("oi", [1,2])` → `a=String, b=[Int]` → retorna `String`

**Aprendizado:**
- `:i` = informações completas (módulo, tipo, instâncias)
- `:t` = apenas o tipo da função
- `fst` é polimórfica - funciona com tuplas de quaisquer tipos

#### 6. Informações sobre `tail`
```haskell
> :i tail
```
**Resultado:** Mostra informações sobre a função `tail`

> **O que você verá:**
> ```
> tail :: [a] -> [a]  -- Defined in 'GHC.List'
> ```
> - Definida no módulo de listas padrão
> - Função do Prelude (sempre disponível)

```haskell
> :t tail
```
**Resultado:** `tail :: [a] -> [a]`

> **Explicação do tipo `[a] -> [a]`:**
>
> - `[a]` = entrada: uma lista de elementos do tipo `a`
>   - `a` pode ser qualquer tipo
>   - Todos elementos da lista devem ser do mesmo tipo
> - `->` = retorna
> - `[a]` = saída: outra lista do mesmo tipo `a`
>
> **Por que o tipo é o mesmo na entrada e saída?**
> Porque `tail` apenas remove o primeiro elemento, mas não muda o tipo!
>
> **Exemplos:**
> - `tail [1,2,3]` → entrada `[Int]`, saída `[Int]`
> - `tail "hello"` → entrada `[Char]`, saída `[Char]`
> - `tail [True,False]` → entrada `[Bool]`, saída `[Bool]`

**Aprendizado:** `tail` é polimórfica - funciona com listas de qualquer tipo.

#### 7. Função `tail` com lista de inteiros
```haskell
> tail [6,7,3,9]
```
**Resultado:** `[7,3,9]`
**Tipo:** `it :: Num a => [a]` (ou `[Integer]`)

**Aprendizado:** `tail` remove o primeiro elemento da lista.

> **Explicação detalhada:**
>
> - `tail` remove o primeiro elemento (`6`) e retorna o resto
> - Entrada: `[6,7,3,9]` (4 elementos)
> - Saída: `[7,3,9]` (3 elementos)
>
> **Por que `Num a => [a]` e não `[Int]`?**
> - Os números `6,7,3,9` são literais numéricos
> - Poderiam ser Int, Integer, Float, Double, etc.
> - O Haskell mantém genérico: "lista de números"
> - Se você usar esse resultado em operação com Int, ele vira `[Int]`
> - Se usar com Double, vira `[Double]`
>
> **Cuidado:** `tail []` causa ERRO! (lista vazia não tem cauda)

#### 8. Função `tail` com string
```haskell
> tail "sdferta"
```
**Resultado:** `"dferta"`
**Tipo:** `it :: [Char]`

**Aprendizado:**
- String é uma lista de caracteres: `String = [Char]`
- `tail` funciona com strings da mesma forma que com outras listas

> **Explicação detalhada:**
>
> - `"sdferta"` é equivalente a `['s','d','f','e','r','t','a']`
> - `tail` remove o primeiro caractere ('s')
> - Retorna `"dferta"` = `['d','f','e','r','t','a']`
>
> **Por que `[Char]` e não `Num a => [a]`?**
> - Caracteres NÃO são números!
> - 's', 'd', 'f' são do tipo `Char` (concreto)
> - Não tem restrição `=>` porque o tipo é específico
>
> **Equivalências em Haskell:**
> ```haskell
> "abc" = ['a','b','c']  -- String = [Char]
> String                  -- é um alias para [Char]
> tail "abc" = "bc"       -- funciona igual a listas
> ```
>
> **Funções úteis com strings:**
> - `head "abc"` → `'a'` (primeiro caractere)
> - `tail "abc"` → `"bc"` (resto da string)
> - `length "abc"` → `3` (tamanho)
> - `reverse "abc"` → `"cba"` (inverter)

### Conceitos Importantes Aprendidos

#### 1. Inferência de Tipos
O Haskell descobre automaticamente o tipo de cada expressão. Com `:set +t`, podemos ver esse tipo sendo mostrado após cada avaliação.

#### 2. Polimorfismo
Funções polimórficas funcionam com múltiplos tipos:
- `fst :: (a, b) -> a` - funciona com qualquer tupla
- `tail :: [a] -> [a]` - funciona com qualquer lista

As variáveis de tipo (`a`, `b`, `c`) representam "qualquer tipo" e são substituídas pelos tipos concretos quando a função é usada.

#### 3. Comandos úteis do GHCi
- `:set +t` - ativa exibição de tipos
- `:t <função>` - mostra o tipo de uma função
- `:i <função>` - mostra informações completas sobre uma função
- `:q` - sair do GHCi

## Tarefa 2 - Inferência de Tipos e Avaliação de Expressões

### Objetivo
Inferir o tipo de cada expressão e avaliá-la no GHCi. Praticar a compreensão de tipos compostos e operações com listas.

### Expressões para Avaliar

#### 1. Lista de expressões booleanas
```haskell
> [True, (5>4), (not ('5'=='6')), (True || (False && True))]
```
**Resultado:** `[True, True, True, True]`
**Tipo:** `[Bool]`

**Avaliação passo a passo:**
- `True` → `True`
- `5>4` → `True` (5 é maior que 4)
- `not ('5'=='6')` → `not False` → `True`
- `True || (False && True)` → `True || False` → `True`

> ** Explicação detalhada:**
>
> **Tipo: `[Bool]`**
> - Lista de booleanos
> - Todos os elementos devem ser do mesmo tipo
> - Cada elemento é uma expressão booleana que é avaliada
>
> **Operadores usados:**
> - `>` :: `Ord a => a -> a -> Bool` (comparação)
> - `==` :: `Eq a => a -> a -> Bool` (igualdade)
> - `not` :: `Bool -> Bool` (negação)
> - `||` :: `Bool -> Bool -> Bool` (OR lógico)
> - `&&` :: `Bool -> Bool -> Bool` (AND lógico)
>
> **Precedência dos operadores:**
> 1. Comparações: `>`, `==`
> 2. `not`
> 3. `&&` (AND)
> 4. `||` (OR)
>
> **Por que funciona?**
> - Todas as expressões resultam em `Bool`
> - Uma lista só aceita elementos do mesmo tipo
> - `[Bool]` = lista de booleanos

#### 2. Tupla com tail e head
```haskell
> ((tail "abcdef"), (head "abcdef"))
```
**Resultado:** `("bcdef", 'a')`
**Tipo:** `([Char], Char)` ou `(String, Char)`

> **Explicação detalhada:**
>
> **Tipo: `([Char], Char)`**
> - Tupla com 2 elementos de tipos DIFERENTES
> - Primeiro elemento: `[Char]` (string/lista de caracteres)
> - Segundo elemento: `Char` (um caractere)
>
> **O que cada função faz:**
> - `tail "abcdef"` → `"bcdef"` :: `[Char]`
>   - Remove o primeiro caractere
>   - Retorna uma string (lista de chars)
> - `head "abcdef"` → `'a'` :: `Char`
>   - Pega o primeiro caractere
>   - Retorna um único caractere
>
> **Tuplas vs Listas:**
> - Tuplas: podem ter elementos de tipos diferentes
> - Listas: todos elementos devem ser do mesmo tipo
> - Por isso `(tail ..., head ...)` funciona em tupla!

#### 3.  Lista com tail e head (ERRO!)
```haskell
> [(tail "abcdef"), (head "abcdef")]
```
**Resultado:** ERRO DE TIPO!

```
error:
    • Couldn't match type 'Char' with '[Char]'
      Expected: [Char]
        Actual: Char
```

> **Por que dá erro?**
>
> **Tentativa de criar:** `[([Char], Char)]` ← IMPOSSÍVEL!
>
> - `tail "abcdef"` → `"bcdef"` :: `[Char]`
> - `head "abcdef"` → `'a'` :: `Char`
> - Lista `[]` exige que TODOS elementos sejam do mesmo tipo
> - `[Char]` ≠ `Char` → Tipos diferentes!
>
> **Comparação:**
> ```haskell
> --  FUNCIONA (tupla)
> ((tail "abc"), (head "abc"))  :: ([Char], Char)
>
> --  ERRO (lista)
> [(tail "abc"), (head "abc")]  -- tipos diferentes!
>
> --  FUNCIONA (lista de strings)
> [(tail "abc"), (tail "def")]  :: [[Char]]
>
> --  FUNCIONA (lista de chars)
> [(head "abc"), (head "def")]  :: [Char]
> ```
>
> **Lição importante:**
> Listas = elementos homogêneos (mesmo tipo)
> Tuplas = elementos heterogêneos (tipos diferentes OK)

#### 4. Concatenação de listas
```haskell
> [4,5,6] ++ [3,5,8]
```
**Resultado:** `[4,5,6,3,5,8]`
**Tipo:** `Num a => [a]`

> **Explicação detalhada:**
>
> **Operador `++`**
> - Tipo: `(++) :: [a] -> [a] -> [a]`
> - Concatena (junta) duas listas
> - Ambas listas devem ser do mesmo tipo
>
> **Tipo: `Num a => [a]`**
> - Literais numéricos são polimórficos
> - `[4,5,6]` poderia ser `[Int]`, `[Integer]`, `[Float]`, etc.
> - Resultado mantém o tipo genérico "lista de números"
>
> **Exemplos com `++`:**
> ```haskell
> [1,2] ++ [3,4]        → [1,2,3,4]
> "abc" ++ "def"        → "abcdef"
> [True] ++ [False]     → [True, False]
> [1,2] ++ [3,4] ++ [5] → [1,2,3,4,5]
>
> --  ERRO: tipos diferentes
> [1,2] ++ ['a','b']    → ERRO!
> ```
>
> **Complexidade:**
> - `++` percorre a primeira lista inteira
> - Adicionar no final é lento: O(n)
> - Adicionar no início é rápido: `x:[1,2,3]` → O(1)

#### 5. Tail de lista pequena
```haskell
> (tail [6,7])
```
**Resultado:** `[7]`
**Tipo:** `Num a => [a]`

> **Explicação detalhada:**
>
> **O que acontece:**
> - Lista original: `[6,7]` (2 elementos)
> - `tail` remove o primeiro: `6`
> - Resultado: `[7]` (1 elemento)
>
> **Observações importantes:**
> ```haskell
> tail [1,2,3]  → [2,3]    -- lista com 2 elementos
> tail [1,2]    → [2]      -- lista com 1 elemento
> tail [1]      → []       -- lista vazia
> tail []       → ERRO!    -- Exception: empty list
> ```
>
> **Tipo: `Num a => [a]`**
> - Ainda é uma lista de números genéricos
> - Mesmo tendo só 1 elemento, continua sendo lista
> - `[7]` é diferente de `7`
>   - `[7]` :: `Num a => [a]` (lista)
>   - `7` :: `Num a => a` (número)
>
> **Funções relacionadas:**
> - `head [6,7]` → `6` (primeiro elemento)
> - `tail [6,7]` → `[7]` (resto da lista)
> - `init [6,7]` → `[6]` (tudo exceto último)
> - `last [6,7]` → `7` (último elemento)

#### 6. Concatenar lista de strings
```haskell
> concat ["asdf", "bbb", "tyuui", "cccc"]
```
**Resultado:** `"asdfbbbtyuuicccc"`
**Tipo:** `[Char]`

> **Explicação detalhada:**
>
> **Função `concat`**
> - Tipo: `concat :: [[a]] -> [a]`
> - Recebe: lista de listas
> - Retorna: uma única lista (concatenação de todas)
>
> **O que acontece:**
> - Entrada: `["asdf", "bbb", "tyuui", "cccc"]` :: `[[Char]]`
>   - Lista de strings
>   - String = `[Char]`, então temos lista de listas
> - `concat` junta todas as strings
> - Saída: `"asdfbbbtyuuicccc"` :: `[Char]`
>
> **Por que `[Char]` e não `[[Char]]`?**
> - `concat` "achata" um nível de listas
> - `[[Char]]` → `[Char]`
> - `[[[a]]]` → `[[a]]`
>
> **Exemplos com `concat`:**
> ```haskell
> concat [[1,2], [3,4], [5]]     → [1,2,3,4,5]
> concat ["oi", "tudo", "bem"]   → "oitudobem"
> concat [[True], [False]]       → [True, False]
> concat []                      → []
> concat [[]]                    → []
> concat [[], [1], [], [2,3]]    → [1,2,3]
> ```
>
> **Diferença entre `++` e `concat`:**
> ```haskell
> -- ++ junta DUAS listas
> [1,2] ++ [3,4]              → [1,2,3,4]
>
> -- concat junta LISTA de listas (quantas forem)
> concat [[1,2], [3,4], [5]]  → [1,2,3,4,5]
>
> -- Equivalência:
> concat [[1], [2], [3]]  ==  [1] ++ [2] ++ [3]
> ```

### Resumo dos Conceitos Aprendidos

#### 1. Listas vs Tuplas
| Característica | Lista `[a]` | Tupla `(a, b)` |
|---------------|-------------|----------------|
| Tipos dos elementos | Todos iguais | Podem ser diferentes |
| Tamanho | Variável | Fixo (parte do tipo) |
| Exemplo | `[1,2,3]` | `(1, 'a', True)` |

#### 2. Operadores de Lista
- `++` :: `[a] -> [a] -> [a]` - concatena duas listas
- `concat` :: `[[a]] -> [a]` - concatena lista de listas
- `head` :: `[a] -> a` - primeiro elemento
- `tail` :: `[a] -> [a]` - resto da lista (sem o primeiro)
- `init` :: `[a] -> [a]` - tudo exceto o último
- `last` :: `[a] -> a` - último elemento

#### 3. Erros Comuns
- `tail []` - ERRO! (lista vazia)
- `head []` - ERRO! (lista vazia)
- `[1, 'a']` - ERRO! (tipos diferentes em lista)
- `(1, 'a')` - OK! (tupla aceita tipos diferentes)

## Tarefa 3 - Criação e Uso de Módulos

### Objetivo
Criar um módulo Haskell, carregá-lo no GHCi, verificar os tipos das funções definidas e avaliar expressões.

### Passos da Tarefa

#### Passo 1: Criar o arquivo do módulo

Crie um arquivo chamado `Teste.hs` com o seguinte conteúdo:

```haskell
module Teste where

funcao1 x y = x + (70 * y)

ex a = 50 * a
```

> **Explicação do código:**
>
> **Declaração do módulo:**
> - `module Teste where` - declara que este é o módulo "Teste"
> - O nome do módulo DEVE corresponder ao nome do arquivo (Teste.hs)
> - `where` indica que as definições vêm a seguir
>
> **Função `funcao1`:**
> - Recebe dois parâmetros: `x` e `y`
> - Calcula: `x + (70 * y)`
> - Tipo inferido: `Num a => a -> a -> a`
>   - Aceita dois números do mesmo tipo
>   - Retorna um número do mesmo tipo
>
> **Função `ex`:**
> - Recebe um parâmetro: `a`
> - Calcula: `50 * a`
> - Tipo inferido: `Num a => a -> a`
>   - Aceita um número
>   - Retorna um número do mesmo tipo

#### Passo 2: Carregar o módulo no GHCi

```haskell
> :l Teste.hs
```

**Resultado esperado:**
```
[1 of 1] Compiling Teste           ( Teste.hs, interpreted )
Ok, one module loaded.
```

> **Explicação do comando `:l`:**
>
> - `:l` é abreviação de `:load`
> - Carrega um módulo do arquivo
> - O GHCi compila o arquivo e torna as funções disponíveis
> - Se houver erros de sintaxe, eles serão mostrados aqui
>
> **Comandos relacionados:**
> ```haskell
> :l Teste.hs    -- carrega o módulo
> :r             -- recarrega o módulo (após editar o arquivo)
> :reload        -- mesmo que :r
> :m             -- descarrega todos os módulos
> ```
>
> **Dica importante:**
> - Sempre que modificar o arquivo Teste.hs, use `:r` para recarregar
> - O GHCi precisa estar no mesmo diretório do arquivo, ou você precisa fornecer o caminho completo

#### Passo 3: Verificar os tipos das funções

```haskell
> :t funcao1
```
**Resultado:** `funcao1 :: Num a => a -> a -> a`

> **Explicação do tipo `Num a => a -> a -> a`:**
>
> **Quebrando o tipo:**
> - `Num a =>` - restrição: `a` deve ser um tipo numérico
> - `a ->` - primeiro argumento do tipo `a`
> - `a ->` - segundo argumento do tipo `a` (mesmo tipo!)
> - `a` - resultado do tipo `a` (mesmo tipo!)
>
> **Leitura:**
> "funcao1 é uma função que recebe dois números do mesmo tipo e retorna um número desse mesmo tipo"
>
> **Por que todos são do mesmo tipo `a`?**
> - A operação `x + (70 * y)` exige que todos sejam do mesmo tipo
> - Se `x` é Int, então `y` também deve ser Int
> - Se `x` é Double, então `y` também deve ser Double
>
> **Exemplos de uso:**
> ```haskell
> funcao1 5 2       -- a = Integer, resultado: 5 + (70*2) = 145
> funcao1 3.0 1.0   -- a = Double, resultado: 3.0 + 70.0 = 73.0
> funcao1 5 2.5     -- ERRO! Tipos misturados
> ```

```haskell
> :t ex
```
**Resultado:** `ex :: Num a => a -> a`

> **Explicação do tipo `Num a => a -> a`:**
>
> **Quebrando o tipo:**
> - `Num a =>` - restrição: `a` deve ser numérico
> - `a ->` - recebe um argumento do tipo `a`
> - `a` - retorna um valor do tipo `a` (mesmo tipo!)
>
> **Leitura:**
> "ex é uma função que recebe um número e retorna um número do mesmo tipo"
>
> **Por que o tipo de entrada e saída são iguais?**
> - A operação `50 * a` preserva o tipo
> - Se entrada é Int, saída é Int
> - Se entrada é Float, saída é Float
>
> **Exemplos de uso:**
> ```haskell
> ex 10      -- a = Integer, resultado: 50 * 10 = 500
> ex 2.5     -- a = Double, resultado: 50 * 2.5 = 125.0
> ex (-3)    -- a = Integer, resultado: 50 * (-3) = -150
> ```

#### Passo 4: Avaliar a expressão `funcao1 (ex 10) 1`

```haskell
> funcao1 (ex 10) 1
```
**Resultado:** `570`
**Tipo:** `Num a => a`

**Avaliação passo a passo:**

```haskell
funcao1 (ex 10) 1
⇒ funcao1 (50 * 10) 1          -- expande ex 10
⇒ funcao1 500 1                -- calcula 50 * 10
⇒ 500 + (70 * 1)               -- expande funcao1 500 1
⇒ 500 + 70                     -- calcula 70 * 1
⇒ 570                          -- calcula 500 + 70
```

> **Explicação detalhada:**
>
> **Ordem de avaliação:**
> 1. `ex 10` é avaliado primeiro (argumento da função)
>    - `ex 10 = 50 * 10 = 500`
> 2. `funcao1 500 1` é avaliado
>    - `funcao1 500 1 = 500 + (70 * 1) = 500 + 70 = 570`
>
> **Por que `ex 10` é avaliado primeiro?**
> - Em Haskell, os argumentos são avaliados antes de aplicar a função
> - `(ex 10)` está entre parênteses, então é avaliado como uma sub-expressão
> - O resultado (500) é então passado como primeiro argumento de `funcao1`
>
> **Composição de funções:**
> - Esta expressão demonstra **composição de funções**
> - O resultado de uma função (`ex`) é usado como entrada de outra (`funcao1`)
> - Muito comum em programação funcional!
>
> **Verificando os tipos:**
> ```haskell
> ex 10           :: Num a => a      -- retorna um número
> funcao1 500 1   :: Num a => a      -- aceita dois números
> ```
> Os tipos são compatíveis!

### Conceitos Importantes Aprendidos

#### 1. Estrutura de um Módulo Haskell

```haskell
module NomeModulo where

-- Declarações de tipos (opcional mas recomendado)
funcao :: Tipo

-- Definições de funções
funcao parametros = expressao
```

**Regras:**
- Nome do módulo = nome do arquivo (sem .hs)
- Primeira letra do módulo deve ser maiúscula
- Use `where` após o nome do módulo

#### 2. Comandos do GHCi para Módulos

| Comando | Descrição | Exemplo |
|---------|-----------|---------|
| `:l arquivo.hs` | Carrega um módulo | `:l Teste.hs` |
| `:load arquivo.hs` | Mesma coisa (versão longa) | `:load Teste.hs` |
| `:r` | Recarrega o módulo atual | `:r` |
| `:reload` | Mesma coisa (versão longa) | `:reload` |
| `:m` | Descarrega todos os módulos | `:m` |
| `:show modules` | Mostra módulos carregados | `:show modules` |
| `:t funcao` | Mostra tipo de uma função | `:t funcao1` |
| `:i funcao` | Informações sobre função | `:i ex` |

#### 3. Inferência de Tipos

O Haskell infere automaticamente os tipos das funções baseado em:
- Operações usadas (`+`, `*`, etc.)
- Tipos dos literais (números, strings)
- Como as variáveis são usadas

**Exemplo:**
```haskell
-- Haskell infere automaticamente:
soma x y = x + y
-- Tipo: Num a => a -> a -> a

-- Porque usa +, que tem tipo: (+) :: Num a => a -> a -> a
```

#### 4. Composição de Funções

Usar o resultado de uma função como argumento de outra:

```haskell
funcao1 (ex 10) 1
-- ex 10 é avaliado primeiro
-- Resultado é passado para funcao1
```

**Outros exemplos:**
```haskell
head (tail "abcd")        -- tail primeiro, depois head
length (reverse [1,2,3])  -- reverse primeiro, depois length
not (5 > 3)               -- comparação primeiro, depois not
```

### Exercícios Adicionais (Opcional)

Experimente no GHCi após carregar o módulo:

```haskell
-- 1. Teste outras combinações
> ex 5
> funcao1 10 2
> funcao1 (ex 5) (ex 1)

-- 2. Verifique os tipos
> :t ex 10
> :t funcao1 (ex 10)
> :t funcao1 (ex 10) 1

-- 3. Tente provocar erros
> funcao1 "texto" 1        -- O que acontece?
> ex True                  -- E aqui?
```

## Tarefa 4 - Definição e Teste de Funções

### Objetivo
Definir funções que trabalham com tuplas, comparações e strings. Criar um módulo `Ficha1.hs` para organizar todas as definições.

### Preparação: Criar o módulo Ficha1

Crie um arquivo `Ficha1.hs` que conterá todas as funções desta tarefa.

### Exercício 1: Operações com Pares

**Enunciado:**
Defina uma função que receba dois pares de inteiros e retorne um par de inteiros, sendo:
- Primeiro elemento: soma dos primeiros elementos dos pares de entrada
- Segundo elemento: produto dos segundos elementos dos pares de entrada

**Solução:**

```haskell
somaEProduto :: (Int, Int) -> (Int, Int) -> (Int, Int)
somaEProduto (a, b) (c, d) = (a + c, b * d)
```

> **Explicação detalhada:**
>
> **Tipo da função:**
> - `(Int, Int)` - primeiro par de entrada
> - `(Int, Int)` - segundo par de entrada
> - `(Int, Int)` - par resultado
>
> **Pattern matching nas tuplas:**
> - `(a, b)` - extrai o primeiro par em `a` e `b`
> - `(c, d)` - extrai o segundo par em `c` e `d`
> - Isso nos dá acesso direto aos 4 valores
>
> **Construção do resultado:**
> - `(a + c, b * d)` - cria novo par
>   - Primeiro elemento: `a + c` (soma dos primeiros)
>   - Segundo elemento: `b * d` (produto dos segundos)

**Exemplos de uso:**

```haskell
> somaEProduto (3, 5) (2, 4)
(5, 20)
```

**Avaliação passo a passo:**
```haskell
somaEProduto (3, 5) (2, 4)
⇒ (3 + 2, 5 * 4)    -- substituição dos valores
⇒ (5, 20)           -- cálculos
```

**Mais exemplos:**
```haskell
> somaEProduto (10, 2) (5, 3)
(15, 6)              -- 10+5=15, 2*3=6

> somaEProduto (0, 7) (0, 8)
(0, 56)              -- 0+0=0, 7*8=56

> somaEProduto (-3, 4) (3, 5)
(0, 20)              -- -3+3=0, 4*5=20
```

### Exercício 2: Maior e Segundo Maior

**Enunciado:**
Escreva uma função que, dados três números inteiros, retorne um par contendo:
- Primeiro elemento: o maior dos números
- Segundo elemento: o segundo maior dos números

**Solução SIMPLIFICADA:**

```haskell
maiorSegundoMaior :: Int -> Int -> Int -> (Int, Int)
maiorSegundoMaior x y z
    | maior == x = (x, max y z)
    | maior == y = (y, max x z)
    | otherwise  = (z, max x y)
    where maior = max x (max y z)
```

> **Explicação detalhada:**
>
> **Estratégia mais simples:**
> 1. Primeiro, encontra qual é o maior dos três
> 2. Depois, encontra o maior dos dois restantes
>
> **Função `max`:**
> - `max :: Ord a => a -> a -> a`
> - Retorna o maior entre dois valores
> - Exemplo: `max 5 3` → `5`
>
> **Passo a passo:**
> - `maior = max x (max y z)` - encontra o maior dos três
>   - Primeiro calcula `max y z`
>   - Depois compara resultado com `x`
> - Se `maior == x`, o segundo maior é `max y z`
> - Se `maior == y`, o segundo maior é `max x z`
> - Se `maior == z`, o segundo maior é `max x y`
>
> **Por que é mais simples?**
> - Usa apenas 3 guards em vez de 6
> - Usa função `max` que é pré-definida
> - Lógica mais clara: "acha o maior, depois acha o maior do resto"

**Exemplos de uso:**

```haskell
> maiorSegundoMaior 5 3 8
(8, 5)
```

**Avaliação passo a passo:**
```haskell
maiorSegundoMaior 5 3 8
-- where: maior = max 5 (max 3 8) = max 5 8 = 8
-- Testa: maior == x? → 8 == 5? Não
-- Testa: maior == y? → 8 == 3? Não
-- otherwise: (8, max 5 3) = (8, 5)
⇒ (8, 5)
```

**Mais exemplos:**
```haskell
> maiorSegundoMaior 10 20 15
(20, 15)

> maiorSegundoMaior 7 7 3
(7, 7)

> maiorSegundoMaior 1 2 3
(3, 2)
```

### Exercício 3: Ordenar Triplo Decrescente

**Enunciado:**
Escreva uma função que receba um triplo de números inteiros e retorne um triplo em que os mesmos números estão ordenados por ordem decrescente.

**Solução SIMPLIFICADA:**

```haskell
ordenaTriplo :: (Int, Int, Int) -> (Int, Int, Int)
ordenaTriplo (x, y, z) = (maior, medio, menor)
    where
        maior = max x (max y z)
        menor = min x (min y z)
        medio = x + y + z - maior - menor
```

> **Explicação detalhada:**
>
> **Estratégia mais simples:**
> 1. Encontra o maior dos três usando `max`
> 2. Encontra o menor dos três usando `min`
> 3. Calcula o médio: soma total menos maior e menor
>
> **Funções usadas:**
> - `max :: Ord a => a -> a -> a` - retorna o maior entre dois valores
> - `min :: Ord a => a -> a -> a` - retorna o menor entre dois valores
>
> **Como funciona:**
> - `maior = max x (max y z)` - compara x com o máximo entre y e z
> - `menor = min x (min y z)` - compara x com o mínimo entre y e z
> - `medio = x + y + z - maior - menor` - truque matemático!
>   - Se temos 3 números, a soma de todos menos os extremos = o do meio
>
> **Por que é mais simples?**
> - Não precisa de guards complexos
> - Usa funções pré-definidas (`max` e `min`)
> - Truque matemático elegante para achar o médio
> - Mais curto e legível

**Exemplos de uso:**

```haskell
> ordenaTriplo (5, 2, 8)
(8, 5, 2)
```

**Avaliação passo a passo:**
```haskell
ordenaTriplo (5, 2, 8)
-- x=5, y=2, z=8
-- maior = max 5 (max 2 8) = max 5 8 = 8
-- menor = min 5 (min 2 8) = min 5 2 = 2
-- medio = 5 + 2 + 8 - 8 - 2 = 15 - 10 = 5
⇒ (8, 5, 2)
```

**Mais exemplos:**
```haskell
> ordenaTriplo (3, 9, 1)
(9, 3, 1)

> ordenaTriplo (7, 7, 7)
(7, 7, 7)

> ordenaTriplo (10, 5, 15)
(15, 10, 5)
```

### Exercício 4: Validação de Triângulo

**Enunciado:**
Os lados de qualquer triângulo respeitam a seguinte restrição: a soma dos comprimentos de quaisquer dois lados é superior ao comprimento do terceiro lado. Escreva uma função que receba o comprimento de três segmentos de reta e retorne um valor booleano indicando se satisfazem esta restrição.

**Solução:**

```haskell
trianguloValido :: Double -> Double -> Double -> Bool
trianguloValido a b c = (a + b > c) && (a + c > b) && (b + c > a)
```

> **Explicação detalhada:**
>
> **Tipo da função:**
> - `Double -> Double -> Double` - três lados do triângulo
> - `Bool` - True se forma triângulo válido, False caso contrário
>
> **Desigualdade triangular:**
> Para três lados `a`, `b`, `c` formarem um triângulo válido:
> - `a + b > c` (soma de dois lados > terceiro lado)
> - `a + c > b` (soma de dois lados > terceiro lado)
> - `b + c > a` (soma de dois lados > terceiro lado)
>
> Todas as três condições devem ser verdadeiras (por isso usamos `&&`)
>
> **Por que `Double` e não `Int`?**
> - Medidas podem ser decimais (ex: 3.5 cm)
> - `Double` é mais apropriado para medidas reais
>
> **Operadores usados:**
> - `>` maior que (estrito)
> - `&&` AND lógico (ambos devem ser verdadeiros)

**Exemplos de uso:**

```haskell
> trianguloValido 3 4 5
True
```

**Avaliação passo a passo:**
```haskell
trianguloValido 3 4 5
⇒ (3 + 4 > 5) && (3 + 5 > 4) && (4 + 5 > 3)
⇒ (7 > 5) && (8 > 4) && (9 > 3)
⇒ True && True && True
⇒ True
```

**Mais exemplos:**
```haskell
> trianguloValido 1 2 5
False                -- 1+2=3 não é > 5

> trianguloValido 3 3 3
True                 -- triângulo equilátero

> trianguloValido 5 12 13
True                 -- triângulo retângulo

> trianguloValido 1 1 2
False                -- 1+1=2 não é > 2 (precisa ser estritamente maior)

> trianguloValido 2.5 3.7 4.2
True
```

> **Casos especiais:**
> - Lados com valor 0 ou negativo → resultado False (correto!)
> - Lados iguais → pode ser True (triângulos equiláteros ou isósceles)
> - A soma exata de dois lados igual ao terceiro → False (seria uma linha reta, não triângulo)

### Exercício 5: Abreviar Nome

**Enunciado:**
Escreva uma função `abrev` que receba uma string contendo nome de uma pessoa e retorne uma string com o primeiro nome e apelido.

Exemplo: `abrev "Joao Carlos Martins Sarmento" = "Joao Sarmento"`

Funções úteis:
- `words :: String -> [String]` - divide texto em lista de palavras
- `unwords :: [String] -> String` - junta lista de palavras em texto

**Solução:**

```haskell
abrev :: String -> String
abrev nome
    | length palavras == 1 = head palavras
    | otherwise = head palavras ++ " " ++ last palavras
    where palavras = words nome
```

> **Explicação detalhada:**
>
> **Tipo da função:**
> - `String -> String` - recebe nome completo, retorna abreviado
>
> **Cláusula where:**
> - `palavras = words nome` - cria variável local com a lista de palavras
> - Evita chamar `words nome` múltiplas vezes, tornando o código mais eficiente
>
> **Guards (condições):**
> - `| length palavras == 1` - se só tem uma palavra no nome
>   - `= head palavras` - retorna essa palavra (ex: "Maria" → "Maria")
> - `| otherwise` - caso contrário (2 ou mais palavras)
>   - `= head palavras ++ " " ++ last palavras` - retorna primeira e última
>
> **Funções usadas:**
> - `words nome` - divide o nome em lista de palavras
>   - `words "Joao Carlos Martins Sarmento"` → `["Joao", "Carlos", "Martins", "Sarmento"]`
>   - `words "Maria"` → `["Maria"]`
>
> - `length palavras` - conta quantas palavras tem na lista
>   - `length ["Maria"]` → `1`
>   - `length ["Joao", "Carlos", "Martins", "Sarmento"]` → `4`
>
> - `head palavras` - pega primeira palavra da lista
>   - `head ["Joao", "Carlos", "Martins", "Sarmento"]` → `"Joao"`
>
> - `last palavras` - pega última palavra da lista
>   - `last ["Joao", "Carlos", "Martins", "Sarmento"]` → `"Sarmento"`
>
> - `++` - operador de concatenação de strings
>   - `"Joao" ++ " " ++ "Sarmento"` → `"Joao Sarmento"`

**Exemplos de uso:**

```haskell
> abrev "Joao Carlos Martins Sarmento"
"Joao Sarmento"

> abrev "Maria"
"Maria"
```

**Avaliação passo a passo (nome com várias palavras):**
```haskell
abrev "Joao Carlos Martins Sarmento"
⇒ {where: palavras = words "Joao Carlos Martins Sarmento"}
⇒ {where: palavras = ["Joao", "Carlos", "Martins", "Sarmento"]}
⇒ {verifica guard: length ["Joao", "Carlos", "Martins", "Sarmento"] == 1}
⇒ {verifica guard: 4 == 1} → False
⇒ {usa otherwise}
⇒ head ["Joao", "Carlos", "Martins", "Sarmento"] ++ " " ++ last ["Joao", "Carlos", "Martins", "Sarmento"]
⇒ "Joao" ++ " " ++ "Sarmento"
⇒ "Joao Sarmento"
```

**Avaliação passo a passo (nome com uma palavra):**
```haskell
abrev "Maria"
⇒ {where: palavras = words "Maria"}
⇒ {where: palavras = ["Maria"]}
⇒ {verifica guard: length ["Maria"] == 1}
⇒ {verifica guard: 1 == 1} → True
⇒ head ["Maria"]
⇒ "Maria"
```

**Mais exemplos:**
```haskell
> abrev "Maria Silva"
"Maria Silva"        -- já tem só 2 nomes

> abrev "Ana Paula Costa Santos"
"Ana Santos"

> abrev "Pedro"
"Pedro Pedro"        -- cuidado: só 1 nome repete!

> abrev "Jose Antonio da Silva"
"Jose Silva"
```

> **Problema potencial:**
> Se o nome tiver apenas uma palavra, `head` e `last` retornam a mesma,
> resultando em repetição. Poderíamos melhorar:
>
> ```haskell
> abrevMelhorado :: String -> String
> abrevMelhorado nome
>     | length palavras == 1 = head palavras
>     | otherwise = unwords [head palavras, last palavras]
>     where palavras = words nome
> ```

### Resumo dos Conceitos Aprendidos

#### 1. Pattern Matching em Tuplas

```haskell
funcao (a, b) = ...           -- extrai elementos de par
funcao (x, y, z) = ...        -- extrai elementos de triplo
```

Permite acessar diretamente os elementos das tuplas.

#### 2. Guards (Guardas)

```haskell
funcao x
    | condicao1 = resultado1
    | condicao2 = resultado2
    | otherwise = resultado3
```

Alternativa ao `if-then-else` para múltiplas condições.

#### 3. Cláusula `where`

```haskell
funcao x = expressao
    where
        variavel1 = valor1
        variavel2 = valor2
```

Define variáveis locais para uso na função.

#### 4. Funções de Listas Úteis

| Função | Tipo | Descrição |
|--------|------|-----------|
| `head` | `[a] -> a` | Primeiro elemento |
| `last` | `[a] -> a` | Último elemento |
| `words` | `String -> [String]` | Divide texto em palavras |
| `unwords` | `[String] -> String` | Junta palavras em texto |

#### 5. Operadores Relacionais e Lógicos

| Operador | Descrição |
|----------|-----------|
| `>` | Maior que |
| `>=` | Maior ou igual |
| `<` | Menor que |
| `<=` | Menor ou igual |
| `==` | Igual a |
| `/=` | Diferente de |
| `&&` | AND lógico |
| `||` | OR lógico |
| `not` | Negação |

### Testando o Módulo Ficha1

Após criar o arquivo `Ficha1.hs` com todas as funções:

```haskell
> :l Ficha1.hs
> :t somaEProduto
> somaEProduto (5, 3) (2, 4)
> maiorSegundoMaior 10 5 8
> ordenaTriplo (3, 9, 1)
> trianguloValido 3 4 5
> abrev "Ana Maria Santos"
```

## Tarefa 5 - Importação de Módulos

### Objetivo
Aprender a importar e usar módulos em Haskell, tanto módulos de biblioteca (Data.Char) quanto módulos personalizados criados por você.

### Parte 1: Criando e Testando o Módulo Conv1

#### 1. Criar o arquivo Conv1.hs

Crie um arquivo `Conv1.hs` com o seguinte conteúdo:

```haskell
module Conv1 where

import Data.Char

con :: Char
con = toLower 'A'

fun :: Char -> Char
fun x = toUpper x
```

> **Explicação do código:**
>
> **Importação de módulo de biblioteca:**
> - `import Data.Char` - importa o módulo Data.Char
> - Este módulo contém funções para manipular caracteres
> - Funções disponíveis: `toLower`, `toUpper`, `isAlpha`, `isDigit`, etc.
>
> **Constante `con`:**
> - Tipo: `Char` (caractere)
> - Valor: `toLower 'A'` → `'a'`
> - `toLower` converte maiúscula para minúscula
>
> **Função `fun`:**
> - Tipo: `Char -> Char`
> - Recebe um caractere e retorna sua versão maiúscula
> - `toUpper` converte minúscula para maiúscula

#### 2. Carregar Conv1 no GHCi

```haskell
> :l Conv1.hs
```

**Resultado esperado:**
```
[1 of 1] Compiling Conv1           ( Conv1.hs, interpreted )
Ok, one module loaded.
```

> **O que aconteceu:**
> - O GHCi compilou o módulo Conv1
> - Agora podemos usar `con` e `fun`
> - O módulo Data.Char também está disponível

#### 3. Testar a constante con

```haskell
> con
```

**Resultado:** `'a'`
**Tipo:** `it :: Char`

> **Explicação:**
> - `con` é uma constante definida como `toLower 'A'`
> - `toLower 'A'` converte 'A' para 'a'
> - Resultado: `'a'`

#### 4. Verificar o tipo de con

```haskell
> :t con
```

**Resultado:** `con :: Char`

> **Explicação do tipo:**
> - `con` tem tipo `Char` (caractere)
> - É uma constante, não uma função
> - Sempre retorna o mesmo valor: `'a'`

#### 5. Testar a função fun

```haskell
> fun 'b'
```

**Resultado:** `'B'`
**Tipo:** `it :: Char`

> **Explicação:**
> - `fun` recebe um caractere
> - `toUpper` converte para maiúscula
> - `fun 'b'` → `toUpper 'b'` → `'B'`

**Mais exemplos:**
```haskell
> fun 'x'
'X'

> fun 'Z'
'Z'           -- já é maiúscula, permanece igual

> fun '5'
'5'           -- número, não tem maiúscula
```

#### 6. Verificar o tipo de fun

```haskell
> :t fun
```

**Resultado:** `fun :: Char -> Char`

> **Explicação do tipo:**
> - `fun` é uma função
> - Recebe: `Char` (um caractere)
> - Retorna: `Char` (um caractere)

#### 7. Testar funções do Data.Char

Após carregar Conv1, temos acesso ao Data.Char:

```haskell
> toLower 'M'
```

**Resultado:** `'m'`

```haskell
> toUpper 'd'
```

**Resultado:** `'D'`

```haskell
> isAlpha 'a'
```

**Resultado:** `True`

```haskell
> isDigit '7'
```

**Resultado:** `True`

> **Funções úteis do Data.Char:**
> - `toLower :: Char -> Char` - converte para minúscula
> - `toUpper :: Char -> Char` - converte para maiúscula
> - `isAlpha :: Char -> Bool` - verifica se é letra
> - `isDigit :: Char -> Bool` - verifica se é dígito
> - `isSpace :: Char -> Bool` - verifica se é espaço
> - `ord :: Char -> Int` - retorna código ASCII
> - `chr :: Int -> Char` - converte código ASCII para caractere

### Parte 2: Criando e Testando Módulos Conv2 e Exemp

#### 8. Criar o arquivo Conv2.hs

Crie um arquivo `Conv2.hs`:

```haskell
module Conv2 where

import Data.Char

upperandlower :: Char -> [Char]
upperandlower c = [toLower c, toUpper c]
```

> **Explicação do código:**
>
> **Função `upperandlower`:**
> - Tipo: `Char -> [Char]` (recebe caractere, retorna lista de caracteres)
> - Recebe um caractere `c`
> - Retorna lista com duas versões: `[minúscula, maiúscula]`
> - Exemplo: `upperandlower 'A'` → `['a', 'A']`

#### 9. Criar o arquivo Exemp.hs

Crie um arquivo `Exemp.hs`:

```haskell
module Exemp where

import Conv2
import Data.Char

conv :: Char -> [Char]
conv x = if (isAlpha x) then (upperandlower x)
         else []
```

> **Explicação do código:**
>
> **Importações:**
> - `import Conv2` - importa nosso módulo Conv2 (função upperandlower)
> - `import Data.Char` - importa módulo de biblioteca (função isAlpha)
>
> **Função `conv`:**
> - Tipo: `Char -> [Char]`
> - Recebe um caractere `x`
> - Verifica se `x` é letra usando `isAlpha`
> - Se for letra: retorna `upperandlower x` (lista com minúscula e maiúscula)
> - Se não for letra: retorna `[]` (lista vazia)
>
> **Estrutura if-then-else:**
> - `if condição then resultado1 else resultado2`
> - O `else` é OBRIGATÓRIO em Haskell
> - Ambos resultados devem ter o mesmo tipo

#### 10. Carregar Exemp no GHCi

```haskell
> :l Exemp.hs
```

**Resultado esperado:**
```
[1 of 2] Compiling Conv2           ( Conv2.hs, interpreted )
[2 of 2] Compiling Exemp           ( Exemp.hs, interpreted )
Ok, two modules loaded.
```

> **O que aconteceu:**
> - O GHCi detectou que Exemp depende de Conv2
> - Compilou Conv2 primeiro (dependência)
> - Depois compilou Exemp
> - Agora temos acesso às funções de ambos os módulos

#### 11. Verificar módulos carregados

```haskell
> :show modules
```

**Resultado esperado:**
```
Conv2           ( Conv2.hs, interpreted )
Exemp           ( Exemp.hs, interpreted )
```

> **Explicação:**
> - Lista todos os módulos atualmente carregados
> - Conv2 está carregado (dependência de Exemp)
> - Exemp está carregado (módulo principal)

#### 12. Testar a função upperandlower

```haskell
> upperandlower 'A'
```

**Resultado:** `"aA"`
**Tipo:** `it :: [Char]`

> **Explicação:**
> - `upperandlower 'A'` → `[toLower 'A', toUpper 'A']`
> - `[toLower 'A', toUpper 'A']` → `['a', 'A']`
> - Resultado exibido como string: `"aA"`

**Mais exemplos:**
```haskell
> upperandlower 'b'
"bB"

> upperandlower 'Z'
"zZ"

> upperandlower '5'
"55"           -- número não tem maiúscula/minúscula
```

#### 13. Verificar tipo de upperandlower

```haskell
> :t upperandlower
```

**Resultado:** `upperandlower :: Char -> [Char]`

> **Explicação do tipo:**
> - Recebe: `Char` (um caractere)
> - Retorna: `[Char]` (lista de caracteres = String)
> - Sempre retorna lista com 2 elementos

#### 14. Testar a função conv com letra

```haskell
> conv 'a'
```

**Resultado:** `"aA"`
**Tipo:** `it :: [Char]`

> **Avaliação passo a passo:**
> ```haskell
> conv 'a'
> ⇒ if (isAlpha 'a') then (upperandlower 'a') else []
> ⇒ if True then (upperandlower 'a') else []
> ⇒ upperandlower 'a'
> ⇒ [toLower 'a', toUpper 'a']
> ⇒ ['a', 'A']
> ⇒ "aA"
> ```

#### 15. Testar a função conv com não-letra

```haskell
> conv '5'
```

**Resultado:** `""`
**Tipo:** `it :: [Char]`

> **Avaliação passo a passo:**
> ```haskell
> conv '5'
> ⇒ if (isAlpha '5') then (upperandlower '5') else []
> ⇒ if False then (upperandlower '5') else []
> ⇒ []
> ⇒ ""
> ```

**Mais exemplos:**
```haskell
> conv 'X'
"xX"

> conv ' '
""             -- espaço não é letra

> conv '!'
""             -- pontuação não é letra
```

#### 16. Verificar tipo de conv

```haskell
> :t conv
```

**Resultado:** `conv :: Char -> [Char]`

> **Explicação do tipo:**
> - Recebe: `Char` (um caractere)
> - Retorna: `[Char]` (lista de caracteres)
> - Pode retornar lista vazia `[]` ou lista com 2 elementos
> - Mesmo sendo resultados diferentes, o tipo é o mesmo: `[Char]`

#### 17. Informações sobre conv

```haskell
> :i conv
```

**Resultado esperado:**
```
conv :: Char -> [Char]  -- Defined in 'Exemp'
```

> **Explicação:**
> - Mostra onde a função foi definida (módulo Exemp)
> - Mostra o tipo da função
> - `:i` = `:info` (informações completas)

### Resumo

**Conceitos aprendidos:**

1. **Importação de módulos de biblioteca:**
   - `import Data.Char` - módulo padrão do Haskell
   - Dá acesso a funções de manipulação de caracteres

2. **Importação de módulos personalizados:**
   - `import Conv2` - importa módulo que você criou
   - O arquivo Conv2.hs deve estar no mesmo diretório

3. **Dependências entre módulos:**
   - Exemp depende de Conv2
   - O GHCi automaticamente carrega as dependências
   - Ordem de compilação: Conv2 primeiro, depois Exemp

4. **Comandos GHCi para trabalhar com módulos:**
   - `:l arquivo.hs` - carrega módulo
   - `:r` - recarrega módulos (após editar arquivos)
   - `:show modules` - lista módulos carregados
   - `:t função` - mostra tipo de função
   - `:i função` - mostra informações completas

5. **Estrutura if-then-else:**
   - Sintaxe: `if condição then valor1 else valor2`
   - `else` é obrigatório em Haskell
   - Ambos valores devem ter o mesmo tipo

## Tarefa 6 - Explorando o Módulo Data.Char

### Objetivo
Explorar as definições e funções disponíveis no módulo Data.Char usando o comando `:browse` (ou `:b`) do GHCi.

### Comando

```haskell
> :browse Data.Char
```

Ou na forma abreviada:

```haskell
> :b Data.Char
```

> **Explicação do comando:**
>
> - `:browse` (ou `:b`) - comando do GHCi para listar todas as funções e tipos de um módulo
> - `Data.Char` - módulo que contém funções para manipulação de caracteres
> - Mostra a assinatura de tipo de cada função disponível

### Resultado Esperado

O comando mostrará uma lista extensa de funções disponíveis no módulo Data.Char. Algumas das principais:

```haskell
type String = [Char]

-- Funções de conversão
toLower :: Char -> Char
toUpper :: Char -> Char
toTitle :: Char -> Char

-- Funções de teste (predicados)
isAlpha :: Char -> Bool
isDigit :: Char -> Bool
isAlphaNum :: Char -> Bool
isSpace :: Char -> Bool
isLower :: Char -> Bool
isUpper :: Char -> Bool
isPunctuation :: Char -> Bool

-- Conversão entre caracteres e códigos
ord :: Char -> Int
chr :: Int -> Char

-- E muitas outras...
```

> **Principais funções do Data.Char:**
>
> **Funções de Conversão:**
> - `toLower :: Char -> Char` - converte para minúscula
> - `toUpper :: Char -> Char` - converte para maiúscula
> - `toTitle :: Char -> Char` - converte para título (maiúscula especial)
>
> **Funções de Verificação (retornam Bool):**
> - `isAlpha :: Char -> Bool` - verifica se é letra (a-z, A-Z)
> - `isDigit :: Char -> Bool` - verifica se é dígito (0-9)
> - `isAlphaNum :: Char -> Bool` - verifica se é letra ou dígito
> - `isSpace :: Char -> Bool` - verifica se é espaço em branco
> - `isLower :: Char -> Bool` - verifica se é minúscula
> - `isUpper :: Char -> Bool` - verifica se é maiúscula
> - `isPunctuation :: Char -> Bool` - verifica se é pontuação
> - `isControl :: Char -> Bool` - verifica se é caractere de controle
>
> **Conversão ASCII:**
> - `ord :: Char -> Int` - retorna o código ASCII do caractere
> - `chr :: Int -> Char` - retorna o caractere correspondente ao código ASCII

### Exemplos de Uso

Após ver as funções disponíveis com `:b Data.Char`, você pode testá-las:

```haskell
> :m + Data.Char
```

> **O comando `:m + Data.Char`:**
> - `:m` = `:module` (gerenciar módulos carregados)
> - `+` = adicionar módulo
> - Torna as funções do Data.Char disponíveis diretamente

Agora teste as funções:

```haskell
> toLower 'A'
'a'

> toUpper 'hello'
-- ERRO: toUpper espera Char, não String

> isDigit '7'
True

> isDigit 'a'
False

> isAlpha 'x'
True

> isSpace ' '
True

> ord 'A'
65

> chr 65
'A'

> chr 97
'a'
```

### Outros Comandos Úteis para Explorar Módulos

```haskell
> :browse Prelude
```
Lista todas as funções do módulo Prelude (módulo padrão)

```haskell
> :browse Conv1
```
Lista as funções do seu módulo Conv1

```haskell
> :show modules
```
Mostra todos os módulos atualmente carregados

```haskell
> :module
```
Descarrega todos os módulos extras

### Resumo

**Conceitos aprendidos:**

1. **Comando `:browse` (`:b`):**
   - Explora o conteúdo de qualquer módulo
   - Mostra todas as funções e seus tipos
   - Útil para descobrir o que um módulo oferece

2. **Comando `:module` (`:m`):**
   - `:m + ModuleName` - adiciona módulo ao escopo
   - `:m - ModuleName` - remove módulo do escopo
   - `:m` - remove todos os módulos extras

3. **Módulo Data.Char:**
   - Funções essenciais para trabalhar com caracteres
   - Conversões (toLower, toUpper)
   - Verificações (isAlpha, isDigit, etc.)
   - Conversões ASCII (ord, chr)

## Tarefa 7 - Funções com Recursividade e Listas

### Objetivo
Definir funções que trabalham com recursividade, listas e operações matemáticas. Praticar pattern matching e funções pré-definidas de listas.

### Exercício 1: Exponenciação Inteira

**Enunciado:**
Defina uma função que calcule o resultado da exponenciação inteira x^y sem recorrer a funções pré-definidas.

**Solução:**

```haskell
potencia :: Int -> Int -> Int
potencia _ 0 = 1
potencia x y = x * potencia x (y-1)
```

> **Explicação detalhada:**
>
> **Tipo da função:**
> - `Int -> Int -> Int` - recebe base (x) e expoente (y), retorna resultado
>
> **Estratégia recursiva:**
> - **Caso base:** `potencia _ 0 = 1`
>   - Qualquer número elevado a 0 é 1
>   - `_` indica que não importa o valor de x
> - **Caso recursivo:** `potencia x y = x * potencia x (y-1)`
>   - x^y = x * x^(y-1)
>   - Multiplica x pelo resultado de x^(y-1)
>
> **Como funciona a recursão:**
> - A cada chamada, diminui o expoente em 1
> - Quando chega a 0, retorna 1 (caso base)
> - Volta multiplicando x em cada nível
>
> **Limitação:**
> - Só funciona para expoentes não-negativos (y >= 0)
> - Para y negativo, entra em loop infinito!

**Exemplos de uso:**

```haskell
> potencia 2 3
8
```

**Avaliação passo a passo:**
```haskell
potencia 2 3
⇒ 2 * potencia 2 2
⇒ 2 * (2 * potencia 2 1)
⇒ 2 * (2 * (2 * potencia 2 0))
⇒ 2 * (2 * (2 * 1))
⇒ 2 * (2 * 2)
⇒ 2 * 4
⇒ 8
```

**Mais exemplos:**
```haskell
> potencia 5 0
1              -- 5^0 = 1

> potencia 3 4
81             -- 3^4 = 3*3*3*3 = 81

> potencia 10 2
100            -- 10^2 = 10*10 = 100

> potencia 2 10
1024           -- 2^10 = 1024
```

### Exercício 2: Primeiro e Último Elemento

**Enunciado:**
Defina uma função que recebe uma lista e constrói um par com o primeiro e o último elemento da lista.

**Solução:**

```haskell
primeiroUltimo :: [a] -> (a, a)
primeiroUltimo lista = (head lista, last lista)
```

> **Explicação detalhada:**
>
> **Tipo da função:**
> - `[a] -> (a, a)` - recebe lista de tipo genérico, retorna par
> - `a` é variável de tipo (funciona com qualquer tipo)
>
> **Funções usadas:**
> - `head :: [a] -> a` - retorna primeiro elemento
> - `last :: [a] -> a` - retorna último elemento
> - Ambas são pré-definidas no Prelude
>
> **Construção do par:**
> - `(head lista, last lista)` cria tupla com dois elementos
> - Primeiro elemento: primeiro da lista
> - Segundo elemento: último da lista
>
> **ATENÇÃO - Erro com lista vazia:**
> ```haskell
> > primeiroUltimo []
> *** Exception: Prelude.head: empty list
> ```
> - `head` e `last` falham com lista vazia
> - Devemos ter cuidado ao usar esta função!

**Exemplos de uso:**

```haskell
> primeiroUltimo [1,2,3,4]
(1, 4)
```

**Mais exemplos:**
```haskell
> primeiroUltimo [10]
(10, 10)       -- lista com 1 elemento: primeiro = último

> primeiroUltimo ['a', 'b', 'c']
('a', 'c')

> primeiroUltimo "Haskell"
('H', 'l')     -- String é [Char]

> primeiroUltimo [True, False, True]
(True, True)
```

### Exercício 3: Lista com seu Comprimento

**Enunciado:**
Defina uma função que dada uma lista dá o par com essa lista e com o seu comprimento.

**Solução:**

```haskell
listaComprimento :: [a] -> ([a], Int)
listaComprimento lista = (lista, length lista)
```

> **Explicação detalhada:**
>
> **Tipo da função:**
> - `[a] -> ([a], Int)` - recebe lista, retorna par (lista, tamanho)
> - Primeiro elemento do par: a lista original
> - Segundo elemento do par: quantidade de elementos (Int)
>
> **Função usada:**
> - `length :: [a] -> Int` - conta elementos da lista
> - Pré-definida no Prelude
>
> **Construção do resultado:**
> - `(lista, length lista)` - tupla com lista e seu tamanho
> - A lista é retornada sem modificação
> - O comprimento é calculado com `length`
>
> **Funciona com lista vazia:**
> ```haskell
> > listaComprimento []
> ([], 0)
> ```

**Exemplos de uso:**

```haskell
> listaComprimento [1,2,3]
([1,2,3], 3)
```

**Mais exemplos:**
```haskell
> listaComprimento []
([], 0)

> listaComprimento "abc"
("abc", 3)

> listaComprimento [10,20,30,40,50]
([10,20,30,40,50], 5)

> listaComprimento [[1,2], [3,4,5]]
([[1,2], [3,4,5]], 2)    -- 2 sublistas
```

### Exercício 4: Média de Lista de Números

**Enunciado:**
Defina uma função que dada uma lista de números calcula a sua média.

**Solução:**

```haskell
media :: [Double] -> Double
media lista = sum lista / fromIntegral (length lista)
```

> **Explicação detalhada:**
>
> **Tipo da função:**
> - `[Double] -> Double` - recebe lista de números decimais, retorna média
> - Usamos `Double` para permitir resultados decimais precisos
>
> **Funções usadas:**
> - `sum :: Num a => [a] -> a` - soma todos os elementos
> - `length :: [a] -> Int` - conta elementos (retorna Int)
> - `fromIntegral :: (Integral a, Num b) => a -> b` - converte Int para Double
>
> **Por que `fromIntegral`?**
> - `length` retorna `Int`
> - Não podemos dividir `Double / Int` diretamente
> - `fromIntegral` converte Int para Double
> - Aí podemos fazer `Double / Double`
>
> **Fórmula da média:**
> - Média = (soma dos elementos) / (quantidade de elementos)
> - `sum lista` calcula o numerador
> - `fromIntegral (length lista)` calcula o denominador
>
> **ATENÇÃO - Erro com lista vazia:**
> ```haskell
> > media []
> Infinity    -- ou erro de divisão por zero
> ```

**Exemplos de uso:**

```haskell
> media [2, 4, 6]
4.0
```

**Avaliação passo a passo:**
```haskell
media [2, 4, 6]
⇒ sum [2, 4, 6] / fromIntegral (length [2, 4, 6])
⇒ 12.0 / fromIntegral 3
⇒ 12.0 / 3.0
⇒ 4.0
```

**Mais exemplos:**
```haskell
> media [10, 20, 30]
20.0

> media [1, 2, 3, 4, 5]
3.0

> media [7.5, 8.5, 9.0]
8.333333333333334

> media [100]
100.0
```

### Resumo dos Conceitos Aprendidos

#### 1. Recursividade

```haskell
funcao _ casoBase = resultadoBase
funcao x y = expressao (funcao x (y-1))
```

Elementos de uma função recursiva:
- **Caso base:** condição de parada
- **Caso recursivo:** chamada à própria função com argumentos menores
- **Convergência:** garantir que chegamos ao caso base

#### 2. Funções de Lista Pré-definidas

| Função | Tipo | Descrição |
|--------|------|-----------|
| `head` | `[a] -> a` | Primeiro elemento |
| `last` | `[a] -> a` | Último elemento |
| `length` | `[a] -> Int` | Número de elementos |
| `sum` | `Num a => [a] -> a` | Soma de todos elementos |

#### 3. Conversão de Tipos

```haskell
fromIntegral :: (Integral a, Num b) => a -> b
```

Converte tipos inteiros (Int, Integer) para qualquer tipo numérico (Float, Double, etc.)

**Uso comum:**
```haskell
-- Divisão de Int por Int (não funciona diretamente)
resultado = soma / quantidade                    -- ERRO!

-- Solução: converter Int para Double
resultado = fromIntegral soma / fromIntegral quantidade  -- OK!
```

#### 4. Pattern Matching com Wildcard

```haskell
potencia _ 0 = 1
```

O `_` (underscore) significa "qualquer valor" - usamos quando não precisamos usar o valor do parâmetro.

#### 5. Cuidados com Listas Vazias

Algumas funções falham com lista vazia:
- `head []` - ERRO!
- `last []` - ERRO!
- `sum []` - OK! (retorna 0)
- `length []` - OK! (retorna 0)
- `media []` - ERRO! (divisão por zero)

### Testando as Funções

Adicione as funções ao arquivo `Ficha1.hs` e teste no GHCi:

```haskell
> :l Ficha1.hs
> potencia 2 5
32

> primeiroUltimo [10,20,30,40]
(10, 40)

> listaComprimento "Haskell"
("Haskell", 7)

> media [5, 10, 15, 20]
12.5
```

## Guia de Sintaxes do Haskell

### 1. Declaração de Tipos (`::`)

```haskell
expressao :: Tipo
```
Lê-se: "expressão tem tipo Tipo"

**Exemplos:**
```haskell
5 :: Int              -- 5 é do tipo Int
'a' :: Char           -- 'a' é do tipo Char
True :: Bool          -- True é do tipo Bool
soma :: Int -> Int -> Int  -- soma é uma função
```

### 2. Restrições de Tipo (`=>`)

**Sintaxe geral:**
```haskell
Classe a => Tipo
```

Lê-se: "Tipo `a`, onde `a` pertence à classe Classe"

**Principais classes de tipos:**

| Classe | O que significa | Exemplos de tipos |
|--------|----------------|-------------------|
| `Num a` | `a` é um tipo numérico | Int, Integer, Float, Double |
| `Fractional a` | `a` é um tipo decimal | Float, Double |
| `Integral a` | `a` é um tipo inteiro | Int, Integer |
| `Ord a` | `a` pode ser ordenado | Int, Char, Bool, String |
| `Eq a` | `a` pode ser comparado por igualdade | Quase todos os tipos |
| `Show a` | `a` pode ser convertido para String | Quase todos os tipos |

**Exemplos práticos:**

```haskell
-- Tipo simples (sem restrição)
x :: Int
x = 5

-- Com restrição de tipo
y :: Num a => a
y = 10  -- pode ser Int, Integer, Float, Double, etc.

-- Função com restrição
soma :: Num a => a -> a -> a
soma x y = x + y
-- Funciona com qualquer tipo numérico!
```

### 3. Variáveis de Tipo (`a`, `b`, `c`, ...)

Letras minúsculas representam "qualquer tipo".

**Exemplos:**

```haskell
-- a pode ser qualquer tipo
identidade :: a -> a
identidade x = x

-- a e b podem ser tipos diferentes
fst :: (a, b) -> a
fst (x, y) = x

-- Todos elementos devem ser do mesmo tipo
head :: [a] -> a
head (x:xs) = x
```

### 4. Tipos de Funções (`->`)

A seta `->` separa entrada e saída.

**Leitura:**
```haskell
f :: A -> B
```
"f é uma função que recebe um A e retorna um B"

**Múltiplos argumentos:**
```haskell
soma :: Int -> Int -> Int
```
Lê-se da direita para esquerda:
- "soma recebe um Int"
- "e retorna uma função que recebe outro Int"
- "e retorna um Int"

Ou simplesmente: "soma recebe dois Int e retorna um Int"

### 5. Tipos Compostos

**Tuplas:**
```haskell
(Int, Char)           -- tupla com Int e Char
(Bool, Float, String) -- tupla com 3 elementos
(a, b)                -- tupla de tipos quaisquer
```

**Listas:**
```haskell
[Int]     -- lista de inteiros
[Char]    -- lista de caracteres (= String)
[a]       -- lista de elementos do tipo a
[[Int]]   -- lista de listas de inteiros
```

### 6. Exemplos Completos Explicados

#### Exemplo 1:
```haskell
it :: Num a => a
```
**Tradução completa:**
"it tem tipo 'a', onde 'a' é qualquer tipo da classe Num (numérico)"

**Na prática:**
O resultado poderia ser Int, Integer, Float ou Double. O Haskell não decidiu ainda.

#### Exemplo 2:
```haskell
fst :: (a, b) -> a
```
**Tradução completa:**
"fst é uma função que recebe uma tupla de tipos (a, b) e retorna um valor do tipo a"

**Na prática:**
```haskell
fst (5, 'x')     -- a = Int, b = Char, retorna Int
fst (True, 3.5)  -- a = Bool, b = Double, retorna Bool
```

#### Exemplo 3:
```haskell
tail :: [a] -> [a]
```
**Tradução completa:**
"tail é uma função que recebe uma lista de elementos do tipo 'a' e retorna outra lista de elementos do mesmo tipo 'a'"

**Na prática:**
```haskell
tail [1,2,3]      -- a = Int
tail "hello"      -- a = Char
tail [True,False] -- a = Bool
```
