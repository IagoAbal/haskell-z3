{-# LANGUAGE ScopedTypeVariables #-}

module Example.Monad.IntList where

import Z3.Monad
import Control.Monad.IO.Class (liftIO)

run :: IO ()
run = evalZ3 datatypeScript

{- Create a recursive int list datatype

-}
mkIntListDatatype :: Z3 Sort
mkIntListDatatype = do

  intS <- mkIntSort
  
  -- nil constructor
  nil <- mkStringSymbol "nil"
  isNil <- mkStringSymbol "is_nil"

  -- cons constructors
  hd <- mkStringSymbol "hd"
  tl <- mkStringSymbol "tl"
  cons <- mkStringSymbol "cons"
  isCons <- mkStringSymbol "is_cons"

  nilCons <- mkConstructor nil isNil []
  consCons <- mkConstructor cons isCons [ (hd, Just intS, 0), (tl, Nothing, 0) ]

  intList <- mkStringSymbol "IntList"
  
  mkDatatype intList [nilCons, consCons]

printFuncs :: [FuncDecl] -> Z3 ()
printFuncs = mapM_ (\c -> getDeclName c >>= getSymbolString >>=
                          liftIO. putStrLn)

datatypeScript :: Z3 ()
datatypeScript = do

  intList <- mkIntListDatatype

  [nilC, consC] <- getDatatypeSortConstructors intList
  [nilR, consR] <- getDatatypeSortRecognizers intList
  [[],[hdA, tlA]] <- getDatatypeSortConstructorAccessors intList
  
  
  liftIO $ putStrLn "*** List constructors are named (should be nil, cons)"
  printFuncs [nilC, consC]
  liftIO $ putStrLn "*** List recognizers are named"
  printFuncs [nilR, consR]

  liftIO $ putStrLn "*** List accessors are named (should be hd, tl)"
  printFuncs [hdA, tlA]

  let listToAST [] = mkApp nilC []
      listToAST (n:ns) = do
        ns' <- listToAST ns
        nn <- mkInteger n
        mkApp consC [ nn, ns' ]

  nil <- mkApp nilC []
  fortyTwo <- mkInteger 42
  fiftySeven <- mkInteger 57
  l1 <- mkApp consC [ fortyTwo, nil]
  l2 <- mkApp consC [ fiftySeven, nil]

  eightyTwo <- mkInteger 82
  l3 <- mkApp consC [ eightyTwo, l1]
  l4 <- mkApp consC [ eightyTwo, l2]
  
  push
  -- Simple test of the recursive list datatype with mkEq

  liftIO $ putStrLn "*** Is nil != cons 42 nil?  Expect Unsat"
  
  p <- mkNot =<< mkEq nil l1
  mkNot p >>= assert
  
  check >>= liftIO . print
  pop 1

  push 
  {- list-equiv is a recursive function that returns true when two
     integer lists are "equivalent," i.e., when adding one to the elements
     of the first list gives the second. -}
    
  boolS <- mkBoolSort

  -- Build the list-equiv function

  listEquivSym <- mkStringSymbol "list-equiv"

  listEquivF <- mkRecFuncDecl listEquivSym [intList, intList] boolS
  l1s <- mkFreshConst "l1a" intList
  l2s <- mkFreshConst "l2a" intList

  rnil1 <- mkApp nilR [l1s]
  rnil2 <- mkApp nilR [l2s]
  nilPred <- mkAnd [rnil1, rnil2] -- Both lists are nil

  rcons1 <- mkApp consR [l1s] -- First list is cons
  
  rcons2 <- mkApp consR [l2s] -- Second list is cons
  
  hd1 <- mkApp hdA [l1s]
  one <- mkInteger 1
  hd1' <- mkAdd [hd1, one]
  hd2 <- mkApp hdA [l2s]
  hdeq <- mkEq hd1' hd2  -- First head + 1 = second head
  
  tl1 <- mkApp tlA [l1s]
  tl2 <- mkApp tlA [l2s]
  tlequiv <- mkApp listEquivF [tl1, tl2] -- list-equiv tl1 tl2
  
  consPred <- mkAnd [rcons1, rcons2, hdeq, tlequiv]

  equivBody <- mkOr [nilPred, consPred] -- lists are nil or cons and equivalent

  -- Define the body of the function
  addRecDef listEquivF [l1s, l2s] equivBody

  push

  l4 <- listToAST [3,4,5]
  l5 <- listToAST [4,5,6]

  liftIO $ putStrLn "*** Comparing two 'equiv' lists.  Generated SMTLIB2 code:"

  mkApp listEquivF [l4, l5] >>= mkNot >>= assert
  
  solverToString >>= liftIO . putStr

  liftIO $ putStrLn "*** list-equiv [3,4,5] [4,5,6] Expecting Unsat (equiv-list is always true)"  
  check >>= liftIO . print

  pop 1

  let twoListsEquiv l1 l2 = do
        liftIO $ putStrLn $ "*** list-equiv " ++ show l1 ++ " " ++ show l2 ++
          " expecting " ++
          if map (+1) l1 == l2
          then "Unsat (list-equiv is always true)"
          else "Sat (list-equiv can be false)"
        push
        l1' <- listToAST l1
        l2' <- listToAST l2
        mkApp listEquivF [l1', l2'] >>= mkNot >>= assert

        check >>= liftIO . print
        pop 1
        
  twoListsEquiv [] []  -- equiv
  twoListsEquiv [1] [1] -- not equiv
  twoListsEquiv [1] [2] -- equiv
  twoListsEquiv [1] [2,2] -- not equiv
  twoListsEquiv [1,2,3] [2,3,4] -- equiv
  twoListsEquiv [1,2,3,4,5,6] [2,3,4,5,6,6] -- not equiv
  twoListsEquiv [1,2,3,4] [2,3,4] -- not equiv
  twoListsEquiv [1,2,3,4,5,5,6] [2,3,4,5,6,6,7] -- equiv
  
  pop 1
   
  return ()
