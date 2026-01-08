# Aula 04

## Recursividade em Haskell

A recursividade é uma técnica fundamental em programação funcional, onde uma função se chama a si mesma para resolver um problema. Em Haskell, a recursividade é amplamente utilizada devido à natureza declarativa da linguagem.

### Definição de Função Recursiva

Uma função recursiva em Haskell é definida com uma ou mais cláusulas que especificam os casos base e os casos recursivos. O caso base é a condição que termina a recursão, enquanto o caso recursivo é onde a função se chama novamente com argumentos modificados.

### Vantagens:
  - Simplicidade e clareza do código.
  - Fácil de entender e implementar.
  - Boa para entender conceitos básicos de recursão.

### Desvantagens:
  - Ineficiência para valores grandes de `n` devido à recomputação de valores já calculados.
  - Pode levar a estouro de pilha para valores muito grandes de `n`.

### Sintaxe Básica

```haskell
nomeDaFuncao :: TipoDoArgumento -> TipoDoRetorno
nomeDaFuncao argumento
	| condicaoBase = valorBase
	| otherwise    = nomeDaFuncao (argumentoModificado)
```

### Exemplo: Sequência de Fibonacci

```haskell
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)
```

### Funções Recursivas Comuns

#### take

A função `take` em Haskell é usada para extrair os primeiros `n` elementos de uma lista. Ela é definida de forma recursiva, onde o caso base lida com a situação em que `n` é zero ou a lista está vazia, e o caso recursivo lida com a extração dos elementos restantes.

```haskell
take :: Int -> [a] -> [a]
take 0 _ = []  -- Caso base: se n é 0, retorna uma lista vazia
take _ [] = [] -- Caso base: se a lista está vazia, retorna uma lista vazia
take n (x:xs) = x : take (n-1) xs -- Caso recursivo: pega o primeiro elemento e chama take para o restante da lista
```

#### tail

A função `tail` em Haskell retorna todos os elementos de uma lista, exceto o primeiro. Ela é definida de forma recursiva, onde o caso base lida com a situação em que a lista está vazia, e o caso recursivo lida com a extração dos elementos restantes.

```haskell
tail :: [a] -> [a]
tail [] = []          -- Caso base: se a lista está vazia, retorna uma lista vazia
tail (_:xs) = xs     -- Caso recursivo: retorna a cauda da lista
```

#### zipWith

A função `zipWith` em Haskell combina dois listas aplicando uma função a pares de elementos correspondentes. Ela é definida de forma recursiva, onde o caso base lida com a situação em que uma das listas está vazia, e o caso recursivo lida com a aplicação da função aos elementos das listas.

```haskell
zipWith :: (a -> b -> c) -> [a] -> [b] ->
zipWith _ [] _ = []          -- Caso base: se a primeira lista está vazia, retorna uma lista vazia
zipWith _ _ [] = []          -- Caso base: se a segunda lista está vazia, retorna uma lista vazia
zipWith f (x:xs) (y:ys) = f x y : zipWith f xs ys -- Caso recursivo: aplica a função aos primeiros elementos e chama zipWith para o restante das listas
```

## Outras conceitos importantes

### Utilização de _ para ignorar argumentos

Em Haskell, o caractere `_` pode ser usado para ignorar argumentos que não são necessários para a implementação da função. Isso é útil quando você quer definir uma função que não utiliza todos os seus parâmetros.

### Uso da sintaxe f (h : t)

Em Haskell, a sintaxe `f (h : t)` é usada para definir funções que operam em listas. Aqui, `h` representa o primeiro elemento (cabeça) da lista, e `t` representa o restante da lista (cauda). Essa sintaxe é útil para decompor listas em seus componentes e aplicar operações recursivas sobre elas.






