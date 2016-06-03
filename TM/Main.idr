module TM.Main

import TM.TM
import TM.Types
import TM.Alphabet

main : IO ()
main = do putStrLn "Enter input please: "
          x <- getLine
          let s = process x
          print s
