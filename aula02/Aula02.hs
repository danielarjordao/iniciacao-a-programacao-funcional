import Data.Char (ord)
import Data.Bool(not)

numberPi :: Float
numberPi = 3.14159
area :: Float -> Float
area r = numberPi * r * r

areaTriangulo :: Float -> Float -> Float
areaTriangulo b h = (b * h) / 2

g :: Char -> Bool
g x = not (20 > ord x)

main :: IO ()
main = do
  putStrLn "=== Exemplos de uso ==="
  putStrLn $ "Área do círculo com raio 5: " ++ show (area 5)
  putStrLn $ "Área do triângulo (base 10, altura 5): " ++ show (areaTriangulo 10 5)
  putStrLn $ "g 'A': " ++ show (g 'A')

