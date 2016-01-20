module Simple

import Effects
import Effect.File
import Effect.StdIO

main : Eff () [FILE_IO (), STDIO]
main = do True <- open "simplein" ReadWrite
               | False => putStrLn "Error"
          close
