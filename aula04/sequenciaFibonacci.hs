module SequenciaFibonacci where

{-
    Outra forma de definir a sequência de Fibonacci através de zipWith e tail:
-}

{-
    Passo a passo da função sequenciaFibonacci para n = 6:
    lista = 0 : 1 : zipWith (+) lista (tail lista)
          = 0 : 1 : zipWith (+) (0 : 1 : zipWith (+) lista (tail lista)) (1 : zipWith (+) lista (tail lista))
          = 0 : 1 : (0 + 1) : zipWith (+) (1 : zipWith (+) lista (tail lista)) (zipWith (+) lista (tail lista))
          = 0 : 1 : 1 : (1 + 1) : zipWith (+) (zipWith (+) lista (tail lista)) (zipWith (+) lista (tail lista))
          = 0 : 1 : 1 : 2 : (1 + 2) : zipWith (+) (zipWith (+) lista (tail lista)) (zipWith (+) lista (tail lista))
          = 0 : 1 : 1 : 2 : 3 : (2 + 3) : zipWith (+) (zipWith (+) lista (tail lista)) (zipWith (+) lista (tail lista))
          = 0 : 1 : 1 : 2 : 3 : 5 : (3 + 5) : zipWith (+) (zipWith (+) lista (tail lista)) (zipWith (+) lista (tail lista))
          = 0 : 1 : 1 : 2 : 3 : 5 : 8 : ...
    Portanto, sequenciaFibonacci 6 = [0, 1, 1, 2, 3, 5, 8]

-}

sequenciaFibonacci :: Int -> [Int]
sequenciaFibonacci n = take (n + 1) lista
  where
    lista = 0 : 1 : zipWith (+) lista (tail lista)



main :: IO ()
main = do
    print (sequenciaFibonacci 10)
