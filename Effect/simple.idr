module Main

import Effects
import Effect.File
import Effect.StdIO

main : IO ()  --Eff () [FILE_IO (OpenFile ReadWrite), STDIO]
main = do True <- open "simplein" ReadWrite | False => putStrLn "Error"
          close
