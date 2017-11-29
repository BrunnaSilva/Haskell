module Main where 

--Função mdc
mdc' :: Int -> Int -> Int
mdc' x y 
    | mod x y == 0 = y
    | mod y x == 0 = x
    | x < y = mdc' x (mod y x)
    | x > y = mdc' y (mod x y)

-- Função mmc
mmc' :: Int -> Int -> Int
mmc' x y = div (x*y) (mdc' x y)

-- metodo main que ler dois valores e retorna seu mmc 
main = do 
    x <- getLine
    y <- getLine
    print (mmc' (read x :: Int) (read y :: Int))
  
    






















