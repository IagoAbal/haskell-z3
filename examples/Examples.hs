
module Main where

import qualified Example.Monad.Queens4
import qualified Example.Monad.Queens4All
import qualified Example.Monad.DataTypes
import qualified Example.Monad.FuncModel
import qualified Example.Monad.MutuallyRecursive
import qualified Example.Monad.Quantifiers
import qualified Example.Monad.QuantifierElimination
import qualified Example.Monad.ToSMTLib
import qualified Example.Monad.Tuple
import qualified Example.Monad.ParserInterface

import System.Environment

examples =
  [ ("4queens"
    , Example.Monad.Queens4.run
    )
  , ("all4queens"
     , Example.Monad.Queens4All.run
     )
  , ("datatypes"
    , Example.Monad.DataTypes.run
    )
  , ("funcModel"
    , Example.Monad.FuncModel.run
    )
  , ("mutuallyRecursive"
    , Example.Monad.MutuallyRecursive.run
    )
  , ("quantifiers"
    , Example.Monad.Quantifiers.run
    )
  , ("quantifierElimination"
    , Example.Monad.QuantifierElimination.run
    )
  , ("smtlib"
    , Example.Monad.ToSMTLib.run
    )
  , ("tuple"
    , Example.Monad.Tuple.run
    )
  , ("parserInterface"
    , Example.Monad.ParserInterface.run
    )
  ]

runExample :: String -> IO ()
runExample x = case lookup x examples of
                    Just x  -> x
                    Nothing -> error "Example not found"

listExamples :: IO ()
listExamples = mapM_ (putStrLn . fst) examples

main = do
  args <- getArgs
  if null args
    then listExamples
    else case args !! 0 of
              x      -> runExample x
