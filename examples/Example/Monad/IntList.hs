{-# LANGUAGE ScopedTypeVariables #-}

module Example.Monad.IntList where

import Z3.Monad
import Control.Monad.IO.Class (liftIO)

run :: IO ()
run = evalZ3 datatypeScript

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
  
  
  liftIO $ putStrLn "*** List constructors are named"
  printFuncs [nilC, consC]
  liftIO $ putStrLn "*** List recognizers are named"
  printFuncs [nilR, consR]

  liftIO $ putStrLn "*** List accessors are named"
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

  liftIO $ putStrLn "*** Is nil != cons 42 nil?  Expect Unsat"
  
  p <- mkNot =<< mkEq nil l1
  mkNot p >>= assert
  
  -- solverToString >>= liftIO . putStr

  check >>= liftIO . print

  pop 1


  push

  liftIO $ putStrLn "*** list-equiv test"

  boolS <- mkBoolSort

  listEquivSym <- mkStringSymbol "list-equiv"

  listEquivF <- mkRecFuncDecl listEquivSym [intList, intList] boolS
  l1s <- mkFreshConst "l1a" intList
  l2s <- mkFreshConst "l2a" intList

  rnil1 <- mkApp nilR [l1s]
  rnil2 <- mkApp nilR [l2s]

  nilPred <- mkAnd [rnil1, rnil2]

  rcons1 <- mkApp consR [l1s]
  rcons2 <- mkApp consR [l2s]
  hd1 <- mkApp hdA [l1s]
  hd2 <- mkApp hdA [l2s]
  hdeq <- mkEq hd1 hd2
  tl1 <- mkApp tlA [l1s]
  tl2 <- mkApp tlA [l2s]
  tlequiv <- mkApp listEquivF [tl1, tl2]

  consPred <- mkAnd [rcons1, rcons2, hdeq, tlequiv]

  equivBody <- mkOr [nilPred, consPred]

  addRecDef listEquivF [l1s, l2s] equivBody

  let twoListsEquiv l1 l2 = do
        liftIO $ putStrLn $ "*** list-equiv " ++ show l1 ++ " " ++ show l2 ++
          " expecting " ++ if l1 == l2 then "Unsat" else "Sat"
        push
        l1' <- listToAST l1
        l2' <- listToAST l2
        mkApp listEquivF [l1', l2'] >>= mkNot >>= assert

        check >>= liftIO . print
        pop 1
        
  push
  
  l4 <- listToAST [3,4,5]
  l5 <- listToAST [3,4,8]

  mkApp listEquivF [l4, l5] >>= mkNot >>= assert

  solverToString >>= liftIO . putStr
  check >>= liftIO . print

  (ch, m) <- getModel

  case m of Just m' -> showModel m' >>= liftIO . putStrLn
            otherwise -> liftIO . print $ ch

  pop 1

  twoListsEquiv [] []
  twoListsEquiv [1] [1]
  twoListsEquiv [1] [1,2]
  twoListsEquiv [1,2,3] [1,2,3]
  twoListsEquiv [1,2,3,4,5,6] [1,2,3,5,5,6]
  twoListsEquiv [1,2,3,4] [1,2,3]
  twoListsEquiv [1,2,3,4,5,5,6] [1,2,3,4,5,5,6]
  pop 1
   
  return ()

{-
  intList <- mkIntListDatatype

  [nil, cons] <- getDatatypeSortConstructors intList

  push
  foo <- mkFreshConst "l" intList

  nil' <- mkApp nil []

  mkEq nil' foo

  check >>= liftIO . print

  solv <- solverToString -- >>= liftIO . putStr
  liftIO $ putStrLn solv

  pop 1

  liftIO $ putStrLn "Hello World"

-}

{-
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
-}
