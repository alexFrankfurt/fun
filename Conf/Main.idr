module Conf.Main

import Conf.Parser.Core

main : IO ()
main = do putStrLn "Running conf"
          str <- getLine
          -- res <- process str
          putStrLn str
