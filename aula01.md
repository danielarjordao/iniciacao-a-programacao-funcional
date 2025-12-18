# Introdução ao Paradigma Funcional

O paradigma funcional é um estilo de programação que trata a computação como a avaliação de funções matemáticas e evita estados mutáveis e dados mutáveis. Ele enfatiza o uso de funções puras, que são funções que, para os mesmos argumentos, sempre retornam o mesmo resultado sem causar efeitos colaterais.

## Objetivos

- Introduzir os conceitos básicos do paradigma funcional.
- Compreender funções puras, tipagem funcional e polimorfismo.
- Desenvolver capacidade de pensar de forma funcional na resolução de problemas.

## Conceitos Básicos

### Programa

Um programa é uma coleção de funções que transformam dados de entrada em dados de saída. No paradigma funcional, o foco está na definição e aplicação dessas funções.

### Classes de linguagens de programação

- Linguagens imperativas: Focam em comandos que mudam o estado do programa (ex: C, Java).
- Linguagens declarativas: conjunto de declarações que descrevem a relação entre o input e o output (ex: Prolog, ML, Haskell).

### Dados

Dados são representações de informações que podem ser manipuladas por funções.
No paradigma funcional, os dados são frequentemente tratados como valores imutáveis. Isso significa que, uma vez criados, os dados não podem ser alterados. Em vez disso, novas versões dos dados são criadas quando modificações são necessárias.

### Operações

Operações referem-se às ações realizadas sobre os dados através de funções.
As operações em programação funcional são realizadas através da aplicação de funções. Funções podem ser combinadas, compostas e passadas como argumentos para outras funções, permitindo uma abordagem modular e reutilizável para a construção de programas.

### Interpretadores e Compiladores

Interpretadores e compiladores são ferramentas que traduzem o código escrito em uma linguagem de programação para uma forma que possa ser executada por um computador.

- **Interpretadores**: Executam o código linha por linha, traduzindo e executando simultaneamente. Exemplos incluem o interpretador Python e o GHCi para Haskell.
- **Compiladores**: Traduzem o código-fonte completo para código de máquina antes da execução. Exemplos incluem o GCC para C/C++ e o GHC para Haskell.

## Haskell

Haskell é uma linguagem de programação funcional pura, conhecida por sua forte tipagem estática e suporte a funções de ordem superior. Ela permite a criação de programas concisos e expressivos, facilitando a implementação de conceitos funcionais.

### Características do Haskell

- **Funções puras**: Todas as funções em Haskell são puras, o que significa que não têm efeitos colaterais.
- **Tipagem estática**: Haskell verifica os tipos em tempo de compilação, reduzindo erros em tempo de execução.
- **Avaliação preguiçosa**: Haskell avalia expressões apenas quando necessário, o que pode levar a melhorias de desempenho e permite a definição de estruturas de dados infinitas.
- **Funções de ordem superior**: Haskell permite que funções sejam passadas como argumentos e retornadas como resultados de outras funções.

### Exemplo de função em Haskell

```haskell
-- Função que calcula o fatorial de um número
factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * factorial (n - 1)
```

Neste exemplo, a função `factorial` é definida de forma recursiva, onde o caso base é quando o número é 0, retornando 1. Para outros valores, a função multiplica o número atual pelo fatorial do número anterior.

### Critérios de sintaxe

- Sintaxe da função: `nomeDaFuncao argumentos = corpoDaFuncao`
- Uso de `=` para definir o corpo da função.
- Indentação é significativa e usada para definir blocos de código.
- Comentários são iniciados com `--` para uma linha ou `{- -}` para múltiplas linhas.

### Ordem de precedência dos operadores

1. Parênteses `()`
2. Exponenciação `^`
3. Multiplicação `*`, Divisão `/`
4. Adição `+`, Subtração `-`

### Tipos de dados básicos

- `Bool`: Tipo booleano, com valores `True` e `False`.
- `Char`: Tipo de caractere, representando um único caractere.
- `Int`: Tipo inteiro, representando números inteiros.
- `Integer`: Tipo inteiro de precisão arbitrária.
- `Float`: Tipo de ponto flutuante, representando números reais com precisão simples.
- `Double`: Tipo de ponto flutuante, representando números reais com precisão dupla.
- `()`: Tipo unit, representando um valor único e vazio.

### Tipos de dados compostos

- Produtos cartesianos (Tuplas): Agrupam múltiplos valores de diferentes tipos.
-- Exemplo: `(Int, Char)` representa uma tupla contendo um inteiro e um caractere.
- Listas: Sequências ordenadas de elementos do mesmo tipo.
-- Exemplo: `[Int]` representa uma lista de inteiros.
- Funções: Mapeiam valores de um tipo para outro.
-- Exemplo: `Int -> Bool` representa uma função que recebe um inteiro e retorna um booleano.

#### Representação

- `::` é usado para declarar o tipo de uma variável ou função.
-- Exemplo: `x :: Int` indica que `x` é do tipo inteiro.
-- Exemplo: `f :: Int -> Bool` indica que `f` é uma função que recebe um inteiro e retorna um booleano.
- Listas são representadas entre colchetes, por exemplo: `[1, 2, 3]`.
-- `Lista de int`: Lista de números inteiros.
- Tuplas são representadas entre parênteses, por exemplo: `(1, 'a')`.
-- `Par (Int, Char)`: Tupla contendo um inteiro e um caractere.

### Condicionais

As expressões condicionais em Haskell são usadas para executar diferentes blocos de código com base em uma condição. A sintaxe básica é a seguinte:

```haskell
if condição then expressão1 else expressão2
```

Aqui, se a `condição` for verdadeira, `expressão1` será avaliada; caso contrário, `expressão2` será avaliada.



