module Aula05 where

{-
 Teste de padrões com tuplas
 Usar o :set +t no GHCi para ver o tipo das expressões
 Exemplo 1:
 > seg (1,'a')
 'a'
 it :: Char

 Exemplo 2:
 > seg (1, 2)
 2
 it :: Num b => b
-}

seg :: (a, b) -> b
seg (x, y) = y

{-
 Definições locais com let...in e where
 Função que divide dois números inteiros e retorna o quociente e o resto
 3 formas de definir a mesma função
-}

dividir x y = (div x y, mod x y)


dividir2 x y = let q = div x y
                   r = mod x y
                in (q, r)

dividir3 x y = (q, r)
    where q = div x y
          r = mod x y


{-
    Uso de let...in e where juntos
    Definições locais complexas

    Entendendo a função a seguir:
-}

exemplo y =
    let k = 100
        g (1,w,z) = w+z
        g (2,w,z) = w-z
        g (_,_,_) = k
    in ((f y) + (f a) + (f b) , g (y,k,c))
    where c = 10
          (a,b) = (3*c, f 2)
          f x = x + 7*c

{-
    Explicação passo a passo:
    1. A função exemplo recebe um argumento y.
    2. Dentro da função, há uma definição let...in que define:
       - k como 100
       - g como uma função que opera em tuplas de três elementos.
    3. A parte in da definição let...in calcula uma tupla:
       - O primeiro elemento é a soma de f y, f a e f b.
       - O segundo elemento é o resultado de g aplicado à tupla (y, k, c).
    4. A seção where define:
       - c como 10
       - (a, b) como a tupla (3*c, f 2)
       - f como uma função que adiciona 7*c ao seu argumento x.
-}

{-
    Formas de definir listas
    Usando padrões para combinar listas
-}

soma3 :: [Int] -> Int
soma3 [] = 0
soma3 (x:y:z:t) = x + y + z
soma3 (x:y:t) = x + y
soma3 (x:t) = x
