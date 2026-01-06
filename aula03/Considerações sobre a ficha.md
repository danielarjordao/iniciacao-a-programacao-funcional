# Considerações sobre a ficha

## **O que acontece sem :set +t**

- Por defeito, o GHCi mostra **apenas o valor**:

```Haskell
ghci\> 5 + 3
8
```

## **O que acontece com :set +t**

Depois de ativares:

```Haskell
ghci\> :set +t
```

o GHCi passa a mostrar **o valor e o tipo**:

```Haskell
ghci\> 5 + 3
8
it :: Num a =\> a
```

Resumo:

Mostra o tipo de cada expressão avaliada

Para voltar ao comportamento normal, fazer: **:set -t**

**O que significa it:: Char?**

O `it :: Char` indica que o valor devolvido e guardado temporariamente pelo GHCi é do tipo `Char`.

**O que significa Num a :: a -\> a ?**

O tipo `a` tem de pertencer à classe `Num`, logo estar Num a. A isto chama-se restrição de tipos.

O "a" é uma variável de tipo. Não é um tipo em concreto (Int, Float
etc..). Significa qualquer tipo, mediante certas restrições.
