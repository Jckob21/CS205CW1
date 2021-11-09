data Btree a = Leaf a | Unary (Btree a) a | Binary (Btree a) a (Btree a)
            deriving Show

ex1 = Unary(Unary(Unary(Unary(Unary(Unary(Unary (Leaf 0) 1)2)3)4)5)6)7
ex2 = Binary (Binary (Leaf (0, "a")) (1,"z") (Leaf (2,"x")))(3,"y")(Binary (Leaf (4,"b"))(5,"c")(Leaf (6,"d")))
ex3 = Binary (Binary (Leaf 0) 1 (Leaf 2)) 3 (Unary (Leaf 4) 5)
ex4 = Binary (Unary (Leaf 1) 2) 3 (Unary (Leaf 4) 5)
ex5 = Binary (Unary (Leaf 1) 2) 3 (Binary (Leaf 4) 5 (Leaf 10))
ex6 = Leaf 10

diff a b = abs (a-b)

complete :: Btree a -> Bool

complete (Binary l x r) = if complete l && complete r && diff (depth l) (depth r) <= 1 && isUnary l
                            then do{
                               if perfect r || isUnary r
                               then False
                               else True
                              }
                            else do{
                                if isUnary l && perfect r 
                                then False
                                else True
                            }

complete (Leaf x) = True
complete (Unary l x) = lastLeaf l

isUnary :: Btree a -> Bool
isUnary (Unary l x) = True
isUnary (Binary l x r) = False
isUnary (Leaf x) = True

lastLeaf :: Btree a -> Bool
lastLeaf (Unary l x) = False
lastLeaf (Binary l x r) = False
lastLeaf (Leaf x) = True

perfect :: Btree a -> Bool

perfect (Binary l x r) = if perfect l && perfect r
                         then True
                         else False
perfect (Unary l x) = False
perfect (Leaf x) = True 
 

depth :: Btree a -> Int
depth (Leaf x) = 0
depth (Binary left x right) = 1 + min (depth left) (depth right)
depth (Unary left x) = 1 + depth left