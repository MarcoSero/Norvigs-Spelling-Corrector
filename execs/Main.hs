{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.ByteString.Char8  as B
import           Control.Monad          (forever, unless)
import           System.IO              (hFlush, stdout)
import           Control.Exception      (catch)
import           System.IO.Error        (isEOFError)
import qualified Spelling               as Sp

repl :: Sp.TrainingDict -> IO ()
repl ws = forever $ do
    B.putStr "> "
    hFlush stdout
    B.getLine >>= B.putStrLn . Sp.correct ws

main :: IO ()
main = do
  catch (Sp.nWords >>= repl) (\e -> unless (isEOFError e) (ioError e))
  B.putStrLn ""
