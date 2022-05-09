nodo = Node 2 [1] [Nil]
nodoLleno = Node 2 [3] [nodo]
nodos123 = Node 2 [2] [Leaf 2 [1], Leaf 2 [3]]

data BTree a = Nil  | Leaf Int [a] | Node Int [a] [BTree a] deriving (Show)


datos :: BTree a -> [BTree a]
datos (Node  _ _ b) = b 


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
add a n = if length ( datos (n)) < cantidad(n)
                     then editar a n 
                     else Nil

nodoVacio = Node 2 [] []
-- crear desde un [a]
create :: [a]-> a -> BTree a
create [] a = Node 1 [a] []
create mat b= add (head(mat)) nodoVacio


-- buscar el maximo 
buscar_max :: (Ord a, Eq a) => BTree a -> a
buscar_max (Leaf _ list) = last list
buscar_max (Node _ _ list) = buscar_max (last list)

-- buscar el minimo
buscar_min :: (Ord a, Eq a) => BTree a -> a
buscar_min (Leaf _ list) = head(list)
buscar_min (Node _ _ list) = buscar_min(head list)

-- imprimir inoorder, preorder y postorder

-- devolver un array de [a].
return_array:: BTree a -> [a]
return_array (Leaf  _ list) = list
return_array (Node _ list listBT) = (list ++ return_array(head listBT)) ++ return_array(head (tail listBT)) 


-- mostrar la cantidad de item en el arbol.

-- mostrar la profundida.


--Mostrar valor (Lo agregue yo)
--mostrar :: BTree a  -> a
--mostrar  (Leaf _ x) =  head x
--mostrar (Node _ lista _) = mostrar(head(lista))


insert :: (Ord a, Eq a) => BTree a -> a -> BTree a
insert t x = if is_full t then insert_non_full (split t) x
                          else insert_non_full t x


insert_non_full :: (Ord a, Eq a) => BTree a -> a -> BTree a
insert_non_full (Nil) x = Leaf 3 [x]
insert_non_full (Leaf m []) x = Leaf m [x]
insert_non_full l@(Leaf m keys@(k:ks)) x
   | x == k = l
   | x < k = Leaf m (x:keys)
   | x > k = Leaf m (k:new_ks)
      where Leaf _ new_ks = insert_non_full (Leaf m ks) x
insert_non_full (Node m [] (t:ts)) x = if is_full t then insert_non_full (split t) x
                                       else Node m [] [(insert_non_full t x)]
insert_non_full n@(Node m keys@(k:ks) trees@(t:ts)) x
  | x == k = n
  | x < k  = if is_full t then insert_non_full (Node m (newK:k:ks) (newT1:newT2:ts)) x
                          else Node m keys ((insert_non_full t x):ts)
  | x > k  = Node m (k:new_ks) (t:new_ts)
    where Node _ new_ks new_ts = insert_non_full (Node m ks ts) x
          Node _ [newK] [newT1, newT2] = split t


split :: (Ord a, Eq a) => BTree a -> BTree a
split (Leaf m keys) = Node m [k] [Leaf m k1, Leaf m k2]
  where k1 = first_half keys
        k:k2 = last_half keys
split (Node m keys trees) = Node m [k] [Node m k1 t1, Node m k2 t2]
  where k1 = first_half keys
        k:k2 = last_half keys
        t1 = first_half trees
        t2 = last_half trees

first_half :: [a] -> [a]
first_half xs = take (div (length xs) 2) xs

last_half :: [a] -> [a]
last_half xs = drop (div (length xs) 2) xs

is_full :: (Ord a, Eq a) => BTree a -> Bool
is_full (Nil) = False
is_full (Leaf m ks)
  | length ks == (2 * m - 1) = True
  | otherwise = False
is_full (Node m ks _)
  | length ks == (2 * m - 1) = True
  | otherwise = False


insertar ::(Ord a,Eq a)=> [a] -> BTree a 
insertar (x:xs) = if (xs == [])
                  then insert Nil x
                  else insert  (insertar (xs)) (x)

