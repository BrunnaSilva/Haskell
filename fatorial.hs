-- Fatorial de um numero usando recursão
factorial :: (Integral x) => x -> x
factorial 0 = 1
factorial x = x*factorial(x-1)