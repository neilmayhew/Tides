{-# LANGUAGE FlexibleContexts #-}

module System.IO.Fake
  ( putStr
  , putStrLn
  , print
  , runIO
  , execIO
  , IO
  ) where

import Control.Monad.Writer.Strict
import Prelude hiding (IO, print, putStr, putStrLn)
import qualified Prelude

type IO = WriterT String Prelude.IO

putStr :: MonadWriter String m => String -> m ()
putStr = tell

putStrLn :: MonadWriter String m => String -> m ()
putStrLn s = putStr s >> putStr "\n"

print :: (Show a, MonadWriter String m) => a -> m ()
print = putStrLn . show

runIO :: IO a -> Prelude.IO (a, String)
runIO = runWriterT

execIO :: IO a -> Prelude.IO String
execIO = execWriterT
