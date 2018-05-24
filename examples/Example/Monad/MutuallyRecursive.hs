{-# LANGUAGE ScopedTypeVariables #-}

module Example.Monad.MutuallyRecursive where

import Z3.Monad
import Control.Monad.IO.Class (liftIO)

run :: IO ()
run = evalZ3 datatypeScript

mkForestTreeDatatypes :: Z3 [Sort]
mkForestTreeDatatypes = do
  -- Creates forest and tree data types of the form:
  -- data Forest = NilF | ConsF {carF :: Tree, cdrF :: Forest}
  -- data Tree = NilT | ConsT {carT :: Forest, cdrT :: Forest}

  -- Nil constructors
  nilF <- mkStringSymbol "NilF"
  isNilF <- mkStringSymbol "is_NilF"
  nilFConst <- mkConstructor nilF isNilF []

  nilT <- mkStringSymbol "NilT"
  isNilT <- mkStringSymbol "is_NilT"
  nilTConst <- mkConstructor nilT isNilT []

  -- Cons constructors
  carF <- mkStringSymbol "carF"
  cdrF <- mkStringSymbol "cdrF"
  consF <- mkStringSymbol "ConsF"
  isConsF <- mkStringSymbol "is_ConsF"

  carT <- mkStringSymbol "carT"
  cdrT <- mkStringSymbol "cdrT"
  consT <- mkStringSymbol "ConsT"
  isConsT <- mkStringSymbol "is_ConsT"

  -- In the following, carT, cdrT, carF, and carT are the field names. The second argument,
  -- their sort, is Nothing, since this is a recursive sort. When we put these in one list
  -- (below) the third argument determines which type they refer to.
  consFConst <- mkConstructor consF isConsF [(carF,Nothing,1),(cdrF,Nothing,0)]
  consTConst <- mkConstructor consT isConsT [(carT,Nothing,0),(cdrT,Nothing,0)]


  -- datatypes (might not need sort list...)
  forest <- mkStringSymbol "Forest"
  tree <- mkStringSymbol "Tree"

  let forestList = [nilFConst, consFConst]
  let treeList = [nilTConst, consTConst]

  mkDatatypes [forest, tree] [forestList, treeList]

datatypeScript :: Z3 ()
datatypeScript = do
  [forest, tree] <- mkForestTreeDatatypes

  [nilF, consF] <- getDatatypeSortConstructors forest
  [nilT, consT] <- getDatatypeSortConstructors tree

  nilF' <- mkApp nilF []

  t1 <- mkApp consT [nilF', nilF']
  f1 <- mkApp consF [t1, nilF']

  liftIO $ putStrLn "prove (NilF != ConsF(ConsT(NilT, NilT), NilF)) //Expect Unsat"
  p <- (mkEq nilF' f1 >>= mkNot)
  push
  mkNot p >>= assert
  check >>= liftIO . print
  pop 1

  liftIO $ putStrLn "prove (consF (x,u) = consF(y,v) => x = y && u = v) //Expect Unsat"
  [x,y] <- mapM (flip mkFreshConst tree) ["x","y"]
  [u,v] <- mapM (flip mkFreshConst forest) ["u","v"]
  f1 <- mkApp consF [x, u]
  f2 <- mkApp consF [y, v]
  p1 <- mkEq f1 f2
  p2 <- mkEq x y
  p3 <- mkEq u v
  p4 <- mkAnd [p2, p3]
  p5 <- mkImplies p1 p4
  push
  mkNot p5 >>= assert
  check >>= liftIO . print
  pop 1
