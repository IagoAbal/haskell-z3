
module Z3.Lang.Monad
  ( Z3 )
  where

import Control.Monad.State ( StateT )

data Z3State

newtype Z3 a = Z3 (StateT Z3State IO a)
