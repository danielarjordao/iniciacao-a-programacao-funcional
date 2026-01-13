# Introdução à Programação Funcional

Material educacional completo sobre programação funcional usando **Haskell**, desenvolvido como disciplina introductória.

## Conteúdo do Repositório

Este repositório contém aulas teóricas, exercícios práticos e soluções resolvidas, organizadas por temas:

### Aula 01 - Conceitos Básicos
**Arquivo:** [`aula01/`](aula01/)
- Exemplos básicos de funções em Haskell
- Operações com listas
- Funções de alta ordem
- **Arquivos:**
  - `Main.hs` - Programa principal com exemplo
  - `Estudos.hs` - Estudo de funções simples e listas

### Aula 02 - Paradigma Funcional
**Arquivo:** [`aula02/`](aula02/)
- Introdução ao paradigma funcional
- Conceitos de funções puras e imutabilidade
- Tipagem em Haskell
- Tipos básicos e compostos
- **Arquivo:** `aula02.md` - Teoria completa com exemplos

### Aula 03 - Ficha Prática 1
**Arquivo:** [`aula03/Ficha1/`](aula03/Ficha1/)
- Valores, expressões e tipos
- Funções e declaração de tipos
- Exercícios práticos resolvidos

**Arquivos:**
- `aula03.md` - Material teórico detalhado
- `Ficha1.hs` - Soluções das tarefas 4 e 7
- `Ficha1_Resolucao.hs` - Resolução oficial completa
- `Ficha1_tarefas.md` - Enunciados e resoluções passo a passo

### Aula 04 - Recursividade
**Arquivo:** [`aula04/`](aula04/)
- Conceitos fundamentais de recursão
- Exemplos práticos: Fibonacci
- Funções recursivas comuns
- Otimizações: recursão linear

**Arquivos:**
- `aula04.md` - Teoria sobre recursividade
- `Fibonacci.hs` - Implementação básica de Fibonacci
- `FibonacciLinear.hs` - Fibonacci otimizado com acumuladores
- `funcoesRecursivasComuns.hs` - take, tail, zipWith
- `sequenciaFibonacci.hs` - Fibonacci com lazy evaluation

### Aula 05 - Ficha Prática 2
**Arquivo:** [`aula05/Ficha2/`](aula05/Ficha2/)
- Pattern matching (concordância de padrões)
- Definições multi-clausais
- Guardas (guards)
- Definições locais (where, let...in)
- Funções recursivas sobre listas
- Type aliases (sinónimos de tipos)

**Arquivos:**
- `aula05.md` - Teoria completa com exemplos
- `Aula05.hs` - Exemplos e testes
- `Ficha2.hs` - Soluções das tarefas
- `Ficha2_tarefas.md` - Enunciados e resoluções

## Como Usar

### Pré-requisitos
- **GHC** (Glasgow Haskell Compiler) instalado

### Instalação do Haskell

#### Linux (Ubuntu/Debian)
```bash
sudo apt-get install ghc
```

### Executar Arquivos

#### Usando GHCi (Interpretador)
```bash
ghci aula01/Main.hs
```

#### Compilar e Executar
```bash
ghc -o meu_programa aula01/Main.hs
./meu_programa
```

#### Executar Diretamente
```bash
runghc aula01/Main.hs
```

## Conceitos Principais

### Programação Funcional
- Funções puras (sem efeitos colaterais)
- Imutabilidade de dados
- Função como valor de primeira classe
- Funções de alta ordem

### Haskell
- Tipagem estática forte
- Inferência de tipos
- Avaliação preguiçosa (lazy evaluation)
- Pattern matching
- Recursividade como mecanismo principal de repetição

## 🎯 Estrutura de Aprendizado

Recomenda-se estudar na seguinte ordem:

1. **Aula 01** - Familiarize-se com a sintaxe básica
2. **Aula 02** - Entenda os conceitos fundamentais
3. **Aula 03** - Pratique com Ficha 1
4. **Aula 04** - Domine recursividade
5. **Aula 05** - Aprofunde com Ficha 2

## Dicas para Estudar

### GHCi
```bash
ghci
> :load aula03/Ficha1/Ficha1.hs
> somaEProduto (3, 5) (2, 4)
(5, 20)
```

Comandos úteis:
- `:load arquivo.hs` - Carrega um arquivo
- `:reload` - Recarrega após edição
- `:type expressao` ou `:t expressao` - Mostra o tipo
- `:info funcao` ou `:i funcao` - Informações da função
- `:set +t` - Mostra tipos dos resultados
- `:quit` ou `:q` - Sair

### Testando Funções
```bash
ghci
> :load aula04/Fibonacci.hs
> fib 6
8
> fib 10
55
```

## Informações Adicionais

Material compilado e organizado para a disciplina de Programação Funcional.

**Última atualização:** Janeiro 2026
