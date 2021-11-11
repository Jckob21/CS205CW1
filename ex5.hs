data Btree a = Leaf a | Unary (Btree a) a | Binary (Btree a) a (Btree a)
            deriving Show

ex1 = Unary(Unary(Unary(Unary(Unary(Unary(Unary (Leaf 0) 1)2)3)4)5)6)7
ex2 = Binary (Binary (Leaf 0) 1 (Leaf 2)) 3 (Unary (Leaf 4) 5)
ex3 = Binary (Unary (Leaf 1) 2) 3 (Unary (Leaf 4) 5)
ex4 = Binary (Unary (Leaf 1) 2) 3 (Binary (Leaf 4) 5 (Leaf 10))
ex5 = Binary (Binary (Leaf (0,"a"))(1,"z")(Leaf (2,"x")))(3,"y")(Binary (Leaf (4,"b"))(5,"c")(Leaf (6,"d")))

-------------------------------------------------------------------------------
-------------------------------------  I  -------------------------------------
-------------------------------------------------------------------------------

-- complete - checks whether a binary tree is a complete tree
-- Parameters:
-- Btree a - type of current node, it can be a binary, unary or leaf
-- Output:
-- Bool - dependly if the tree is a complete then true, otherwise it will produce false

complete :: Btree a -> Bool
complete (Binary l x r) = if complete l && complete r && abs (depth l - depth r) <= 1 && isUnary l
                            then not (perfect r || isUnary r)
                            else not (isUnary l && perfect r)
                            
complete (Leaf x) = True
complete (Unary l x) = lastLeaf l

-- complete - checks whether a binary tree is a complete tree
-- Parameters:
-- Btree a - type of current node, it can be a binary, unary or leaf
-- Output:
-- Bool - dependly if the tree is a complete then true, otherwise it will produce false

isUnary :: Btree a -> Bool
isUnary (Unary l x) = True
isUnary (Binary l x r) = False
isUnary (Leaf x) = True

-- lastLeaf - checks whether a current node is a last leaf
-- Parameters:
-- Btree a - type of current node, it can be a binary, unary or leaf
-- Output:
-- Bool - Outputs True only if the current node (a) is leaf, if it is binary or unary then it is false.

lastLeaf :: Btree a -> Bool
lastLeaf (Unary l x) = False
lastLeaf (Binary l x r) = False
lastLeaf (Leaf x) = True

-- perfect - checks whether a binary tree is a perfect tree
-- Parameters:
-- Btree a - type of current node, it can be a binary, unary or leaf
-- Output:
-- Bool - dependly if the tree is a perfect then true, otherwise it will produce false

perfect :: Btree a -> Bool
perfect (Binary l x r) = perfect l && perfect r
perfect (Unary l x) = False
perfect (Leaf x) = True

-- depth - computes depth for a binary tree or the depth from selected depth
-- Parameters:
-- Btree a - type of current node, it can be a binary, unary or leaf
-- Output:
-- Int - depth of a tree/node

depth :: Btree a -> Int
depth (Leaf x) = 0
depth (Binary left x right) = 1 + min (depth left) (depth right)
depth (Unary left x) = 1 + depth left

-------------------------------------------------------------------------------
-------------------------------------  II  ------------------------------------
-------------------------------------------------------------------------------

-- lookupInSearchTree - Looks up a element which is situated on specific node with first component (k) in the tree
-- Parameters:
-- Integer - (k) First element contained on the current node
-- Btree (Integer, a) - type of current node, it can be a binary, unary or leaf, which contains two elements on the node
-- Output:
-- Maybe a - If there exist some element (v) with first component (k) then It return: Just some element, otherwise if there is no such an element it returns Nothing

lookupInSearchTree :: Integer -> Btree (Integer, a) -> Maybe a

lookupInSearchTree k (Leaf(x, v)) | k == x     = Just y
                                   | otherwise  = Nothing
                                    where y = v


lookupInSearchTree k (Binary l (x,v) r) | k == x       = Just result
                                         | k < x        = lookupInSearchTree k l
                                         | k > x        = lookupInSearchTree k r
                                          where result = v

lookupInSearchTree k (Unary l (x,v)) | k == x      = Just solution
                                      | k < x       = lookupInSearchTree k l
                                       where solution = v

-------------------------------------------------------------------------------
------------------------------------  III  ------------------------------------
-------------------------------------------------------------------------------

-- insertInSearchTree - Inserts another node with two values (Integer,a) to the tree
-- Parameters:
-- Integer - (k) First element to the new node
-- a - second element to the new node
-- Btree (Integer, a) - type of current node, it can be a binary, unary or leaf, which contains two elements on the node
-- Output:
-- Btree (Integer, a) - Outputs all values of the third input of tree type with one copy of new node

insertInSearchTree :: Integer -> a -> Btree (Integer,a) -> Btree (Integer,a)

insertInSearchTree k chr (Leaf(x,y)) = Unary newleft (x,y)
                                       where newleft = Leaf(k,chr)

insertInSearchTree k chr (Binary l (x,y) r) = Binary (insertInSearchTree k chr l) (x,y) r

insertInSearchTree k chr (Unary l (x,y)) =  Binary l (x,y) (Leaf(k,chr))
