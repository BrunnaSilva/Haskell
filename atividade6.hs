module Main where 
    -- Programa Haskell que recebe uma lista de valores inteiros desordenados, insere em uma árvore binária de busca
    -- Retorna : os elementos da arvore em ordem crescente, altura da árvore e quantidade de elementos 
    
    --Função IO
    lerLista :: IO [Int]
    lerLista = do lista <- getLine 
                  return (read lista :: [Int])

    -- Declaração de um tipo árvore 
    data Tree a = TreeEmpty | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)

    --Implementar árvore  
    insertTree :: (Ord a) => a -> Tree a -> Tree a 
    insertTree x TreeEmpty = Node x TreeEmpty TreeEmpty
    insertTree x (Node a left right)
        | x == a = Node x left right 
        | x < a = Node a (insertTree x left) right
        | x > a = Node a left (insertTree x right)

    -- Insere a lista de valores na árvore 
    insertList :: (Ord a) => [a] -> Tree a
    insertList lista = foldr insertTree TreeEmpty lista 

    -- Ordenação dos elementos da árvore 
    sortTree :: (Ord a) => Tree a -> [a]
    sortTree TreeEmpty = []
    sortTree (Node a left right) = (sortTree left) ++ [a] ++ (sortTree right)

    --altura da árvore
    heightTree :: (Ord a) => Tree a -> Int
    heightTree TreeEmpty = -1 
    heightTree (Node a left right) 
        | sideL < sideR = sideR +1
        | otherwise = sideL +1
        where sideL = heightTree left 
              sideR = heightTree right

    -- quantidade de elementos de uma árvore 
    numberElemTree :: (Ord a) => Tree a -> Int 
    numberElemTree TreeEmpty = 0 
    numberElemTree (Node a left right) = 1 + (numberElemTree left) + (numberElemTree right)

    main = do 
       lista <- lerLista
       print (sortTree (insertList lista))
       print (heightTree (insertList lista))
       print (numberElemTree (insertList lista))
       print ("----- -----")
       print ("teste: imprimir árvore")
       print (insertList lista )




