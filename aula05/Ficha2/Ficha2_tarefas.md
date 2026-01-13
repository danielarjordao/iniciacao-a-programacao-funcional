# Ficha 2 - Tarefas e Resoluções

## Instruções
Este documento contém as tarefas da Ficha 2 e suas resoluções explicadas passo a passo.

## Tarefa 1 - Pattern Matching e Ordem das Equações

### Enunciado
Dada a função:
```haskell
f :: (Int, Char, Int) -> Int
f (y, 'a', x) = y + x
f (z, 'b', x) = z * x
f (x, y, z) = x
```

Indique, justificando, o valor das seguintes expressões:
- i) `f (3,'a',5)`
- ii) `f (9,'B',0)`
- iii) `f (5,'b',4)`

O que acontece se alterar a ordem das equações que definem f?

### Resolução

#### i) f (3,'a',5)

**Pattern matching passo a passo:**
1. Testa 1ª equação: `(3,'a',5)` concorda com `(y,'a',x)`?
   - `y = 3`, `'a' = 'a'` ✓, `x = 5`
   - **Concorda!** Usa esta equação: `y + x = 3 + 5 = 8`

**Resposta: 8**

#### ii) f (9,'B',0)

**Pattern matching passo a passo:**
1. Testa 1ª equação: `(9,'B',0)` concorda com `(y,'a',x)`?
   - `'B' ≠ 'a'` ✗ Não concorda!
2. Testa 2ª equação: `(9,'B',0)` concorda com `(z,'b',x)`?
   - `'B' ≠ 'b'` ✗ Não concorda! (maiúscula ≠ minúscula)
3. Testa 3ª equação: `(9,'B',0)` concorda com `(x,y,z)`?
   - `x = 9`, `y = 'B'`, `z = 0` ✓ Concorda! (variáveis aceitam qualquer valor)
   - Usa esta equação: retorna `x = 9`

**Resposta: 9**

#### iii) f (5,'b',4)

**Pattern matching passo a passo:**
1. Testa 1ª equação: `(5,'b',4)` concorda com `(y,'a',x)`?
   - `'b' ≠ 'a'` ✗ Não concorda!
2. Testa 2ª equação: `(5,'b',4)` concorda com `(z,'b',x)`?
   - `z = 5`, `'b' = 'b'` ✓, `x = 4`
   - **Concorda!** Usa esta equação: `z * x = 5 * 4 = 20`

**Resposta: 20**

### O que acontece se alterar a ordem das equações?

**Muito importante!** A ordem das equações é crucial em Haskell!

Se colocarmos o caso genérico primeiro:

```haskell
-- ✗ ORDEM ERRADA!
f (x, y, z) = x             -- Caso genérico PRIMEIRO
f (y, 'a', x) = y + x       -- Nunca será alcançado!
f (z, 'b', x) = z * x       -- Nunca será alcançado!
```

**O que aconteceria:**
- Para qualquer chamada como `f (3,'a',5)`:
  - Testa 1ª equação: `(3,'a',5)` concorda com `(x,y,z)`?
    - ✓ SIM! `x=3`, `y='a'`, `z=5`
    - Retorna `x = 3`
  - **NUNCA chega nas outras equações!**

**Resultado**: Todas as chamadas retornariam apenas o primeiro elemento da tupla, independente do char!

### Regra de Ouro

**Padrões ESPECÍFICOS sempre ANTES de padrões GENÉRICOS!**

- Específicos: constantes (`'a'`, `0`, `True`), estruturas fixas
- Genéricos: variáveis (`x`, `y`), wildcard (`_`)

## Tarefa 2 - Refatorar Função usando Pattern Matching

### Enunciado

Considere a seguinte função:
```haskell
opp :: (Int, (Int, Int)) -> Int
opp z = if ((fst z) == 1)
        then (fst (snd z)) + (snd (snd z))
        else if ((fst z) == 2)
             then (fst (snd z)) - (snd (snd z))
             else 0
```

Defina uma outra versão da função `opp` que tire proveito do mecanismo de **pattern matching**.

Qual das versões lhe parece mais legível?

### Análise da Função Original

A função `opp` recebe uma tupla do tipo `(Int, (Int, Int))`, por exemplo: `(1, (5, 3))`

**O que ela faz:**
- Se o 1º número for `1`: soma os dois números da tupla interna → `5 + 3 = 8`
- Se o 1º número for `2`: subtrai os dois números da tupla interna → `5 - 3 = 2`
- Caso contrário: retorna `0`

**Problema:** A versão original usa muitos `fst` e `snd` aninhados!
- `fst z` → pega o primeiro elemento da tupla principal
- `snd z` → pega a tupla interna `(Int, Int)`
- `fst (snd z)` → pega o primeiro da tupla interna
- `snd (snd z)` → pega o segundo da tupla interna

Muito difícil de ler!

### Resolução

#### Versão com Pattern Matching

```haskell
opp :: (Int, (Int, Int)) -> Int
opp (1, (a, b)) = a + b      -- Se operação é 1: soma
opp (2, (a, b)) = a - b      -- Se operação é 2: subtrai
opp _           = 0          -- Qualquer outro caso: retorna 0
```

**Vantagens:**
- **Muito mais claro!** Desconstruímos a tupla direto nos argumentos
- Cada caso fica em sua própria linha
- Não precisa de `fst` e `snd`
- Variáveis `a` e `b` têm nomes significativos

**Como funciona:**
```haskell
opp (1, (5, 3))
-- Testa 1ª equação: (1, (5, 3)) concorda com (1, (a, b))?
--   1 = 1 ✓, a = 5, b = 3 ✓
-- Usa 1ª equação: a + b = 5 + 3 = 8
⇒ 8

opp (2, (10, 4))
-- Testa 1ª equação: 2 ≠ 1 ✗
-- Testa 2ª equação: (2, (10, 4)) concorda com (2, (a, b))?
--   2 = 2 ✓, a = 10, b = 4 ✓
-- Usa 2ª equação: a - b = 10 - 4 = 6
⇒ 6

opp (5, (7, 2))
-- Testa 1ª equação: 5 ≠ 1 ✗
-- Testa 2ª equação: 5 ≠ 2 ✗
-- Testa 3ª equação: _ concorda com tudo ✓
-- Usa 3ª equação: 0
⇒ 0
```

### Conclusão da Tarefa 2

**Por que pattern matching é melhor aqui:**
- Desconstrução direta da estrutura elimina `fst` e `snd`
- Cada caso fica explícito e separado
- Testa valores específicos (1 e 2) de forma natural
- Código muito mais limpo e legível!

## Tarefa 3 - Definir Funções usando Guardas

### Enunciado

1. Defina novas versões da função `opp` usando definições com **guardas**.
2. Relembre a função `factorial`. Esta definição comporta-se bem sobre números naturais, mas se aplicarmos `factorial` a um número negativo (o que matematicamente não faz sentido), a função **não termina** (entra em loop infinito). Use uma **guarda** na definição de `factorial` para evitar essa situação.

### Parte 1: Versão de `opp` com Guardas

Na Tarefa 2 usamos **pattern matching** para refatorar `opp`. Agora vamos usar **guardas**:

```haskell
oppGuardas :: (Int, (Int, Int)) -> Int
oppGuardas (op, (a, b))
    | op == 1   = a + b      -- Guarda: testa se op é 1
    | op == 2   = a - b      -- Guarda: testa se op é 2
    | otherwise = 0          -- Guarda: caso padrão (otherwise = True)
```

**Como funciona:**
```haskell
oppGuardas (1, (5, 3))
-- Pattern matching: op = 1, a = 5, b = 3
-- Testa 1ª guarda: op == 1? → 1 == 1? ✓ True
-- Usa esta equação: a + b = 5 + 3 = 8
⇒ 8

oppGuardas (2, (10, 4))
-- Pattern matching: op = 2, a = 10, b = 4
-- Testa 1ª guarda: op == 1? → 2 == 1? ✗ False
-- Testa 2ª guarda: op == 2? → 2 == 2? ✓ True
-- Usa esta equação: a - b = 10 - 4 = 6
⇒ 6
```

**Comparando Pattern Matching vs Guardas:**

| Aspecto | Pattern Matching (Tarefa 2) | Guardas (Tarefa 3) |
|---------|------------------------------|---------------------|
| **Sintaxe** | `opp (1, (a, b)) = a + b` | `oppGuardas (op, (a, b)) \| op == 1 = a + b` |
| **Testa** | Valores específicos na estrutura | Condições booleanas |
| **Melhor para** | Desconstrução estrutural, constantes | Comparações, validações numéricas |

Ambas funcionam! A escolha depende do estilo e do que você quer enfatizar.

### Parte 2: Proteger `factorial` com Guardas

#### O Problema

```haskell
-- Versão PERIGOSA
factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n - 1)
```

**O que acontece com números negativos?**

```haskell
factorial (-1)
⇒ (-1) * factorial (-2)
⇒ (-1) * ((-2) * factorial (-3))
⇒ (-1) * ((-2) * ((-3) * factorial (-4)))
-- LOOP INFINITO! Nunca chega a 0!
```

**Por quê?** Com números negativos, subtrair 1 **afasta** cada vez mais de 0 (o caso base), então a recursão **nunca termina**!

#### Solução: Guardas para Validar Entrada

```haskell
factorial :: Int -> Int
factorial n
    | n == 0    = 1                                  -- caso base
    | n > 0     = n * factorial (n - 1)             -- caso recursivo válido
    | otherwise = error "factorial: argumento negativo"
```

**Como funciona:**

```haskell
factorial 3
-- Guarda 1: 3 == 0? ✗ Não
-- Guarda 2: 3 > 0? ✓ Sim!
⇒ 3 * factorial 2
⇒ 3 * (2 * factorial 1)
⇒ 3 * (2 * (1 * factorial 0))
-- Guarda 1: 0 == 0? ✓ Sim!
⇒ 3 * (2 * (1 * 1))
⇒ 6

factorial (-1)
-- Guarda 1: -1 == 0? ✗ Não
-- Guarda 2: -1 > 0? ✗ Não
-- Guarda 3: otherwise? ✓ Sim (sempre True)
⇒ error "factorial: argumento negativo"
-- Programa termina com mensagem de erro clara
```

**Vantagens das guardas aqui:**
- Validação de entrada explícita e legível
- Programa **termina** com erro claro em vez de loop infinito
- Código autodocumentado: só aceita n >= 0
- Cada condição em sua própria linha

### Resumo da Tarefa 3

**Aprendizados sobre Guardas:**
1. Guardas são excelentes para **validar entradas**
2. Use `otherwise` como caso padrão (equivale a `True`)
3. Sempre considere **casos extremos** (negativos, zero, limites)
4. Funções recursivas precisam **sempre convergir** para o caso base
5. Guardas tornam validações **explícitas e autodocumentadas**

**Pattern Matching vs Guardas:**

| Característica | Pattern Matching | Guardas |
|----------------|------------------|---------|
| **Testa** | Estrutura de dados | Condições booleanas |
| **Exemplo** | `opp (1, (a,b)) = ...` | `factorial n \| n > 0 = ...` |
| **Melhor para** | Valores fixos, desconstrução | Comparações, validações |
| **Combinável?** | ✓ Sim! Pode usar ambos juntos | ✓ Sim! Pode usar ambos juntos |

## Tarefa 4: Números de Fibonacci

### Definição Matemática

A sequência de Fibonacci é definida recursivamente:

```
fib(0) = 0
fib(1) = 1
fib(n) = fib(n-2) + fib(n-1)  se n ≥ 2
```

**Sequência gerada:** 0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, ...

### Implementação em Haskell

A tradução direta da definição matemática:

```haskell
fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n - 2) + fib (n - 1)
```

**Exemplo:** Podemos escrever uma outra versão da função Fibonacci equivalente à função que acabou de definir, do seguinte modo:

```haskell
fib 0 = 0
fib 1 = 1
fib (n+2) = fib (n+1) + fib n
```

### Explicação do Padrão `(n+k)`

O padrão `(n+2)` é um **padrão n+k** que funciona assim:

**Como funciona:**
- `(n+2)` faz pattern matching com números **≥ 2**
- Quando recebe um número, **subtrai 2** e guarda o resultado em `n`
- Só faz match se o número for **pelo menos 2**

**Exemplos concretos:**

| Chamada | Pattern matching | Valor de `n` | Explicação |
|---------|------------------|--------------|------------|
| `fib 0` | `fib 0 = 0` | — | Usa 1ª equação (match exato) |
| `fib 1` | `fib 1 = 1` | — | Usa 2ª equação (match exato) |
| `fib 2` | `fib (n+2)` | `n = 0` | 2 = 0+2, então n=0 |
| `fib 3` | `fib (n+2)` | `n = 1` | 3 = 1+2, então n=1 |
| `fib 4` | `fib (n+2)` | `n = 2` | 4 = 2+2, então n=2 |
| `fib 5` | `fib (n+2)` | `n = 3` | 5 = 3+2, então n=3 |

**Avaliação passo a passo de `fib 4`:**

```haskell
fib 4
-- 4 faz match com (n+2), então n = 4-2 = 2
⇒ fib (n+1) + fib n          -- usa 3ª equação
⇒ fib (2+1) + fib 2          -- substitui n=2
⇒ fib 3 + fib 2              -- simplifica

-- Calcula fib 3:
fib 3
-- 3 faz match com (n+2), então n = 3-2 = 1
⇒ fib (1+1) + fib 1          -- substitui n=1
⇒ fib 2 + fib 1              -- simplifica
⇒ fib 2 + 1                  -- usa 2ª equação

-- Calcula fib 2:
fib 2
-- 2 faz match com (n+2), então n = 2-2 = 0
⇒ fib (0+1) + fib 0          -- substitui n=0
⇒ fib 1 + fib 0              -- simplifica
⇒ 1 + 0                      -- usa 1ª e 2ª equações
⇒ 1

-- Junta tudo:
fib 3 = fib 2 + 1 = 1 + 1 = 2
fib 4 = fib 3 + fib 2 = 2 + 1 = 3
```

### Comparação: Versão Original vs Versão com `(n+2)`

| Aspecto | Versão Original | Versão com `(n+2)` |
|---------|----------------|-------------------|
| **Equação recursiva** | `fib n = fib (n-2) + fib (n-1)` | `fib (n+2) = fib (n+1) + fib n` |
| **Para `fib 4`** | `fib (4-2) + fib (4-1)` | `fib (2+1) + fib 2` |
| | `= fib 2 + fib 3` | `= fib 3 + fib 2` |
| **Operação** | Subtração `(n-2)`, `(n-1)` | Adição `(n+1)`, `n` |
| **Lógica** | Pega `n`, calcula anteriores | Pega `n+2`, usa `n` diretamente |

**Ambas são equivalentes!** A diferença é apenas a **perspectiva**:
- **Original:** "Para calcular fib(n), preciso de fib(n-2) e fib(n-1)"
- **Com (n+2):** "Para calcular fib de algo, uso os dois Fibonacci anteriores a ele"

### Por que usar `(n+2)`?

**Vantagens:**
- Evita subtrações `(n-2)` e `(n-1)` no corpo da função
- Usa `n` e `(n+1)` diretamente, mais simples
- Pattern matching garante que `n ≥ 0` (nunca fica negativo)

**Desvantagem:**
- Menos intuitivo para iniciantes
- Padrão n+k é **legado** (deprecated em Haskell moderno)

**Conclusão:** A versão original é mais clara e direta. Use-a!

## Tarefa 5: Definições Locais (`let...in` e `where`)

### Parte 1: Analisar a função `exemplo`

Esta função demonstra o uso combinado de `let...in` e `where`:

```haskell
exemplo :: Int -> (Int, Int)
exemplo y = let k = 100
                g (1,w,z) = w+z
                g (2,w,z) = w-z
                g (_,_,_) = k
            in (f y + f a + f b , g (y,k,c))
    where c = 10
          (a,b) = (3*c, f 2)
          f x = x + 7*c
```

#### Estrutura da função

**Definições no `where`:**
- `c = 10` — constante local
- `(a,b) = (3*c, f 2)` — tupla calculada usando `c` e `f`
- `f x = x + 7*c` — função local que depende de `c`

**Definições no `let...in`:**
- `k = 100` — constante local
- `g` — função local com pattern matching que usa `k`

#### Variável anônima `_`

O padrão `_` (underscore) significa **"não me interessa este valor"**:

```haskell
g (_,_,_) = k  -- ignora os 3 elementos da tupla, retorna k
```

**Uso:**
- Para argumentos que não são utilizados
- Torna o código mais legível (indica intenção explícita)
- Evita warnings de "variável não usada"

**Exemplos:**
```haskell
primeiro (x,_) = x           -- ignora segundo elemento
segundo (_,y) = y            -- ignora primeiro elemento
tamanhoLista (_:xs) = 1 + tamanhoLista xs  -- ignora elemento, usa só a cauda
```

#### Avaliação passo a passo

**Exemplo: `exemplo 1`**

**Passo 1: Avaliar definições no `where`**

```haskell
y = 1
c = 10
f x = x + 7*c = x + 70

-- Calcula a e b:
a = 3*c = 3*10 = 30
b = f 2 = 2 + 70 = 72
```

**Passo 2: Avaliar definições no `let`**

```haskell
k = 100

g (1,w,z) = w+z         -- se primeiro elemento for 1
g (2,w,z) = w-z         -- se primeiro elemento for 2
g (_,_,_) = k           -- qualquer outro caso
```

**Passo 3: Avaliar a expressão após `in`**

Primeira parte da tupla: `f y + f a + f b`
```haskell
f 1 + f 30 + f 72
= 71 + 100 + 142
= 313
```

Segunda parte da tupla: `g (y,k,c) = g (1,100,10)`
```haskell
-- Pattern matching: primeiro elemento é 1, usa 1ª equação
g (1,100,10) = w + z = 100 + 10 = 110
```

**Resultado:**
```haskell
exemplo 1 = (313, 110)
```

**Outro exemplo: `exemplo 2`**

```haskell
-- where e let: mesmos valores (c=10, a=30, b=72, k=100)

f 2 + f 30 + f 72 = 72 + 100 + 142 = 314

-- g (2,100,10): usa 2ª equação (primeiro elemento é 2)
g (2,100,10) = w - z = 100 - 10 = 90

exemplo 2 = (314, 90)
```

**E se o argumento não for 1 nem 2?**

```haskell
exemplo 5

-- g (5,100,10): não é 1 nem 2, usa 3ª equação (catch-all com _)
g (5,100,10) = k = 100

exemplo 5 = (317, 100)
```

#### `let...in` vs `where`

| Aspecto | `let...in` | `where` |
|---------|------------|---------|
| **Posição** | Antes da expressão | Depois da expressão |
| **Escopo** | Apenas na expressão após `in` | Em toda a equação |
| **Uso típico** | Cálculos intermediários localizados | Funções auxiliares, constantes |
| **Pode definir** | Valores e funções | Valores e funções |
| **Ordem** | Top-down (define antes de usar) | Declarativa (ordem não importa) |

**Escolha:**
- Use `where` quando as definições são auxiliares à lógica principal
- Use `let...in` quando o fluxo de cálculo é sequencial

### Parte 2: Raízes de um polinômio

#### O que é um polinômio?

Um **polinômio** é uma equação com potências de x, como:
```
2x² + 3x + 1
x² - 5x + 6
```

Na forma geral: `ax² + bx + c` onde a, b, c são coeficientes.

#### O que são raízes?

**Raízes** são os valores de x que tornam a equação igual a zero.

**Exemplo prático:**

Para `x² - 5x + 6 = 0`, vamos testar x = 2:
```
(2)² - 5(2) + 6 = 4 - 10 + 6 = 0 ✓
```

Testando x = 3:
```
(3)² - 5(3) + 6 = 9 - 15 + 6 = 0 ✓
```

Portanto, as raízes são **x = 2** e **x = 3**.

#### Por que "raízes reais"?

Às vezes não existem números reais que satisfazem a equação, apenas números **imaginários**.

Isso acontece quando o discriminante `Δ = b² - 4ac` é negativo:
- Se `Δ < 0`: não podemos calcular √Δ (raiz quadrada de número negativo)
- Se `Δ ≥ 0`: temos duas raízes reais

#### Fórmula de Bhaskara

Para `ax² + bx + c = 0`:

$$x = \frac{-b \pm \sqrt{b^2 - 4ac}}{2a}$$

onde $\Delta = b^2 - 4ac$ (discriminante)

O símbolo ± significa que temos duas soluções:
- `r1 = (-b + √Δ) / (2a)`
- `r2 = (-b - √Δ) / (2a)`

#### Versão original: `where` com guardas

```haskell
raizes :: (Double,Double,Double) -> (Double,Double)
raizes (a,b,c) = (r1,r2)
    where r1 = (-b + r) / (2*a)
          r2 = (-b - r) / (2*a)
          d = b^2 - 4*a*c
          r | d >= 0 = sqrt d
            | d < 0  = error "raizes imaginarias"
```

**Como funciona:**
1. Calcula discriminante `d = b² - 4ac`
2. Usa guardas em `r` para validar se `d ≥ 0`
3. Se `d < 0`, lança erro (raízes são imaginárias)
4. Se `d ≥ 0`, calcula `r = √d`
5. Retorna `r1` e `r2` usando `r`

**Exemplo: `raizes (1, -5, 6)`**
```haskell
a=1, b=-5, c=6
d = (-5)^2 - 4*1*6 = 25 - 24 = 1
r = sqrt 1 = 1
r1 = (5 + 1) / 2 = 3
r2 = (5 - 1) / 2 = 2
⇒ (3.0, 2.0)
```

#### Versão alternativa: `let...in`

```haskell
raizesLet :: (Double,Double,Double) -> (Double,Double)
raizesLet (a,b,c) =
    let d = b^2 - 4*a*c
        r | d >= 0 = sqrt d
          | d < 0  = error "raizes imaginarias"
        r1 = (-b + r) / (2*a)
        r2 = (-b - r) / (2*a)
    in (r1,r2)
```

**Diferença:** Define tudo no `let` em vez de `where`. Funcionalmente equivalente.

#### Versão sem guardas

```haskell
raizesSemGuardas :: (Double,Double,Double) -> (Double,Double)
raizesSemGuardas (a,b,c) = (r1, r2)
    where r1 = (-b + sqrt d) / (2*a)
          r2 = (-b - sqrt d) / (2*a)
          d = b^2 - 4*a*c
```

**Atenção:** Esta versão **não valida** se `d ≥ 0`. Se `d < 0`, `sqrt d` resulta em `NaN` (Not a Number).

#### Versão compacta (inline)

```haskell
raizesCompacta :: (Double,Double,Double) -> (Double,Double)
raizesCompacta (a,b,c) =
    ( (-b + sqrt (b^2 - 4*a*c)) / (2*a),
      (-b - sqrt (b^2 - 4*a*c)) / (2*a) )
```

**Problema:** Calcula `b² - 4ac` **duas vezes** (ineficiente). Sem validação de erro.

#### Sobre a função `error`

**Tipo:** `error :: String -> a`

**Características:**
- Tipo polimórfico `a` — pode retornar **qualquer tipo**
- Termina o programa com a mensagem fornecida
- Útil para indicar condições impossíveis ou inválidas

**Por que funciona aqui?**
```haskell
r | d >= 0 = sqrt d        -- retorna Double
  | d < 0  = error "..."   -- retorna "a" (qualquer tipo, incluindo Double)
```

O compilador aceita porque `error` pode "fingir" ser qualquer tipo necessário.

### Resumo da Tarefa 5

**Aprendizados:**

1. **Variável anônima `_`:**
   - Usada para argumentos não utilizados
   - Torna intenções explícitas no código

2. **`let...in` vs `where`:**
   - Ambos criam definições locais
   - `where` é mais declarativo, `let...in` é mais sequencial
   - Podem ser combinados na mesma função

3. **Versões alternativas:**
   - Mesma lógica pode ser expressa de formas diferentes
   - Trade-off entre legibilidade, validação e eficiência
   - Guardas permitem validação de entrada

4. **Função `error`:**
   - Tipo `String -> a` (polimórfico)
   - Usado para casos impossíveis/inválidos
   - Termina execução com mensagem


