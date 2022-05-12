

data BTree a = Nil | Leaf Int [a] | Node Int [a] [BTree a] deriving (Show)


-- crear desde un [a]
create ::(Ord a,Eq a)=> [a] -> Int -> BTree a
create [] a = Nil  
create (x:xs) a= if null xs 
                then insert Nil  x
                else insert  (create xs a) x


-- buscar el maximo 
buscarMax :: (Ord a, Eq a) => BTree a -> a
buscarMax (Leaf _ list) = last list
buscarMax (Node _ _ list) = buscarMax (last list)


-- buscar el minimo
buscarMin :: (Ord a, Eq a) => BTree a -> a
buscarMin (Leaf _ list) = head list
buscarMin (Node _ _ list) = buscarMin(head list)


-- imprimir inoorder, preorder y postorder
printPreordenAux :: BTree a -> [[a]]
printPreordenAux Nil  = []
printPreordenAux (Leaf _ a) = [a]
printPreordenAux (Node _ list listBT) = list : printPreorden listBT

printPreorden :: [BTree a] -> [[a]]
printPreorden [] = []
printPreorden (x:xs) = printPreordenAux x ++ printPreorden xs

printInordenAux :: BTree a -> [[a]]
printInordenAux Nil  = []
printInordenAux (Leaf _ a)= [a] 
printInordenAux (Node _ list listBT) =  printInordenAux (head listBT)  ++ list : printInorden (tail listBT) 

printInorden :: [BTree a] -> [[a]]
printInorden [] = []
printInorden (x:xs) =   printInordenAux x ++  printInorden xs

printPostOrdenAux :: BTree a -> [[a]]
printPostOrdenAux Nil  = []
printPostOrdenAux (Leaf _ a) = [a]
printPostOrdenAux (Node _ list listBT) =  printPostorden listBT ++ [list] 

printPostorden :: [BTree a] -> [[a]]
printPostorden [] = []
printPostorden (x:xs) =  printPostOrdenAux x ++ printPostorden xs


-- devolver un array de [a].
returnArrayAuxiliar :: BTree a -> [a]
returnArrayAuxiliar Nil  = []
returnArrayAuxiliar (Leaf _ list) = list
returnArrayAuxiliar (Node x list listBT) =   list ++ returnArray listBT

returnArray :: [BTree a] -> [a]
returnArray [] = []
returnArray (x:xs) =  returnArray xs ++ returnArrayAuxiliar x                                  

quicksort :: Ord a => [a] -> [a]
quicksort []     = []
quicksort (p:xs) = (quicksort lesser) ++ [p] ++ (quicksort greater)
    where
        lesser  = filter (< p) xs
        greater = filter (>= p) xs


-- mostrar la cantidad de item en el arbol.
returnLength :: BTree a -> Int
returnLength nodo = length (returnArray [nodo])


-- mostrar la profundidad.
profundidad :: BTree a -> Int
profundidad (Nil _) = 0
profundidad (Leaf _ _) = 1
profundidad (Node _ _ listBT) = 1 + profundidad(head listBT)


-- Insertar (Auxiliares para poder hacer create)
insert :: (Ord a, Eq a) => BTree a -> a -> BTree a
insert t x = if isFull t then insertNonFull (split t) x
                          else insertNonFull t x

--Cuando cree un Leaf desde un Nil siempre va a tener 3 como limite
insertNonFull :: (Ord a, Eq a) => BTree a -> a -> BTree a
insertNonFull Nil x = Leaf 3 a [x]
insertNonFull (Leaf m []) x = Leaf m [x]
insertNonFull l@(Leaf m keys@(k:ks)) x
   | x == k = l
   | x < k = Leaf m (x:keys)
   | x > k = Leaf m (k:new_ks)
      where Leaf _ new_ks = insertNonFull (Leaf m ks) x
insertNonFull (Node m [] (t:ts)) x = if isFull t then insertNonFull (split t) x
                                       else Node m [] [insertNonFull t x]
insertNonFull n@(Node m keys@(k:ks) trees@(t:ts)) x
  | x == k = n
  | x < k  = if isFull t then insertNonFull (Node m (newK:k:ks) (newT1:newT2:ts)) x
                          else Node m keys (insertNonFull t x:ts)
  | x > k  = Node m (k:new_ks) (t:new_ts)
    where Node _ new_ks new_ts = insertNonFull (Node m ks ts) x
          Node _ [newK] [newT1, newT2] = split t


split :: (Ord a, Eq a) => BTree a -> BTree a
split (Leaf m keys) = Node m [k] [Leaf m k1, Leaf m k2]
  where k1 = firstHalf keys
        k:k2 = lastHalf keys
split (Node m keys trees) = Node m [k] [Node m k1 t1, Node m k2 t2]
  where k1 = firstHalf keys
        k:k2 = lastHalf keys
        t1 = firstHalf trees
        t2 = lastHalf trees

firstHalf :: [a] -> [a]
firstHalf xs = take (div (length xs) 2) xs

lastHalf :: [a] -> [a]
lastHalf xs = drop (div (length xs) 2) xs

isFull :: (Ord a, Eq a) => BTree a -> Bool
isFull Nil = False
isFull (Leaf m ks)
  | length ks == (2 * m - 1) = True
  | otherwise = False
isFull (Node m ks _)
  | length ks == (2 * m - 1) = True
  | otherwise = False



