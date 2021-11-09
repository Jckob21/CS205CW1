data Btree a = Leaf a | Unary (Btree a) a | Binary (Btree a) a (Btree a)
            deriving Show

ex1 = Unary(Unary(Unary(Unary(Unary(Unary(Unary (Leaf 0) 1)2)3)4)5)6)7
ex2 = Binary (Binary (Leaf (0, "a")) (1,"z") (Leaf (2,"x")))(3,"y")(Binary (Leaf (4,"b"))(5,"c")(Leaf (6,"d")))
ex3 = Binary (Binary (Leaf 0) 1 (Leaf 2)) 3 (Unary (Leaf 4) 5)
ex4 = Binary (Unary (Leaf 1) 2) 3 (Unary (Leaf 4) 5)
ex5 = Binary (Unary (Leaf 1) 2) 3 (Binary (Leaf 4) 5 (Leaf 10))
ex6 = Leaf 10
ex7 = Leaf (10, "a")

-------------------------------------------------------------------------------
-------------------------------------  I  -------------------------------------
-------------------------------------------------------------------------------

diff a b = abs (a-b)

complete :: Btree a -> Bool

complete (Binary l x r) = if complete l && complete r && diff (depth l) (depth r) <= 1 && isUnary l
                            then not (perfect r || isUnary r)
                            else not (isUnary l && perfect r)
                            
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

perfect (Binary l x r) = perfect l && perfect r
perfect (Unary l x) = False
perfect (Leaf x) = True


depth :: Btree a -> Int
depth (Leaf x) = 0
depth (Binary left x right) = 1 + min (depth left) (depth right)
depth (Unary left x) = 1 + depth left

-------------------------------------------------------------------------------
-------------------------------------  II  ------------------------------------
-------------------------------------------------------------------------------


-- value t is called "search tree" if the root of every subtree of t is either a leaf or a unary or a binary
--t = Btree (Int, a)

-- All values contained in l have first components <= x
-- Unary l (x, v)

-- All values contained in l have first components <= x, and those contained in r have first components >= x
-- Binary l (x,v) r 


lookupInSearchTree2 :: Integer -> Btree (Integer, a) -> Maybe a

lookupInSearchTree2 k (Leaf(x, v)) | k == x     = Just y
                                   | otherwise  = Nothing
                                    where y = v

--lookupInSearchTree2 k (Leaf(x, _)) = Nothing 


lookupInSearchTree2 k (Binary l (x,v) r) | k == x       = Just result
                                         | k < x        = lookupInSearchTree2 k l
                                         | k > x        = lookupInSearchTree2 k r
                                          where result = v

lookupInSearchTree2 k (Unary l (x,v)) | k == x      = Just solution
                                      | k < x       = lookupInSearchTree2 k l
                                       where solution = v

-------------------------------------------------------------------------------
------------------------------------  III  ------------------------------------
-------------------------------------------------------------------------------

insertInSearchTree :: Integer -> a -> Btree (Integer,a) -> Btree (Integer,a)

insertInSearchTree k chr (Leaf(x,y)) = Unary newleft (x,y)
                                       where newleft = Leaf(k,chr)

insertInSearchTree k chr (Binary l (x,y) r) = insertInSearchTree k chr l

insertInSearchTree k chr (Unary l (x,y)) =  Binary l (x,y) (Leaf(k,chr))
