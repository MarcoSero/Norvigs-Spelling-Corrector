module Main where

import Control.Monad (forever, unless)
import System.IO (hFlush, stdout)
import Control.Exception (catch)
import System.IO.Error (isEOFError)
import Spelling

repl :: TrainingDict -> IO ()
repl ws = forever $ do
    putStr "> "
    hFlush stdout
    getLine >>= putStrLn . correct ws

main :: IO ()
main = do
  catch (nWords >>= repl) (\e -> unless (isEOFError e) (ioError e))
  putStrLn ""
