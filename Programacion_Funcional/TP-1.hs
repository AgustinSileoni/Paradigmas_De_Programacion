{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use foldr" #-}
nodo = Node 2 [1] [Nil]
nodoLleno = Node 2 [3] [nodo]
nodos123 = Node 2 [2] [Leaf 2 [1], Leaf 2 [3]]
arbol = create [1..15]
lista = returnArray [arbol]
arbolMedio = create [1..20]
listaMedia = returnArray [arbolMedio]
arbolGrande = create [1..100]
listaGrande = returnArray [arbolGrande]

data BTree a = Nil  | Leaf Int [a] | Node Int [a] [BTree a] deriving (Show)


datos :: BTree a -> [BTree a]
datos (Node  _ _ b) = b 

datosLeaf :: BTree a -> [a]
datosLeaf (Leaf a list) = list
datosLeaf (Node _ list _) = list

cantidad :: BTree a -> Int
cantidad (Node n _ _) = n

editar :: a -> BTree a-> BTree a
editar x (Node n d b) = Node n d (b++[Leaf 1 [x]])

-- agregar :: a -> BTree a-> [BTree a]
-- agregar x (Node n _ b) =  if length (b) < n 
--                             then (b) ++ [Leaf 1 [x]]
--
--                             else (datos(head(b))) ++ [Leaf 1 [x]]

add :: a -> BTree a -> BTree a
add a (Node x [] _) = Node x [a] [] 
add a n = if length ( datos n) < cantidad n
                     then editar a n 
                     else Nil

nodoVacio = Node 2 [] []


-- crear desde un [a]
create ::(Ord a,Eq a)=> [a] -> BTree a 
create (x:xs) = if null xs 
                  then insert Nil x
                  else insert  (create xs) x

-- buscar el maximo 
buscarMax :: (Ord a, Eq a) => BTree a -> a
buscarMax (Leaf _ list) = last list
buscarMax (Node _ _ list) = buscarMax (last list)

-- buscar el minimo
buscarMin :: (Ord a, Eq a) => BTree a -> a
buscarMin (Leaf _ list) = head list
buscarMin (Node _ _ list) = buscarMin(head list)

-- imprimir inoorder, preorder y postorder

printPreordenAuxiliar :: BTree a -> [[a]]
printPreordenAuxiliar (Leaf _ a) = [a]
printPreordenAuxiliar (Node _ list listBT) = list : printPreorden listBT

printPreorden :: [BTree a] -> [[a]]
printPreorden [] = []
printPreorden (x:xs) = printPreordenAuxiliar x ++ printPreorden xs

printInordenAux :: BTree a -> [[a]]
printInordenAux (Leaf _ a)= [a] 
printInordenAux (Node _ list listBT) =  printInordenAux (head listBT)  ++ list : printInorden (tail listBT) 

printInorden :: [BTree a] -> [[a]]
printInorden [] = []
printInorden (x:xs) =   printInordenAux x ++  printInorden xs


-- devolver un array de [a].
returnArrayAuxiliar :: BTree a -> [a]
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
profundidad (Leaf _ _) = 1
profundidad (Node _ _ listBT) = 1 + profundidad(head listBT)


-- Insertar (Auxiliares para poder hacer create)
insert :: (Ord a, Eq a) => BTree a -> a -> BTree a
insert t x = if isFull t then insertNonFull (split t) x
                          else insertNonFull t x


insertNonFull :: (Ord a, Eq a) => BTree a -> a -> BTree a
insertNonFull Nil x = Leaf 3 [x]
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



