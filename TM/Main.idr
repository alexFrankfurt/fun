module TM.Main

import TM.TM
import TM.Types
import TM.Utils

main : IO ()
main = do putStrLn "Enter input please: "
          x <- getLine
          let (s, r) = process x
          print s
          print r
