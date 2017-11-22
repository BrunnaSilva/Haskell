--Funções implementadas seguindo o tutorial de Tailor Fontela
-- Retorna a cabeça de uma lista 
head' :: [a] -> a
head' [] = error "Empty list"
head' (x:_) = x

-- Comprimento da lista 
length' :: (Num b) => [a] -> b
length' [] = 0
length' (_:xs) = 1 + length' xs

-- Soma dos elementos da lista
sum' :: (Num a) => [a] -> a
sum' [] = 0
sum' (x:xs) = x + sum' xs

--as pattern (@)
firstTerm' :: String -> String
firstTerm' "" = error "Empty string"
firstTerm' name@(x:xs) = "First term of "++ name ++ " is "++ [x]