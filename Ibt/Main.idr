module Ibt.Main

import Effects
import Effect.StdIO
import Effect.File

readProject : { [FILE_IO (), STDIO] } Eff ()
readProject = do putStrLn "Loading project definition..."
                 True <- open "build.ibt" ReadWrite | False => putStrLn "Error."
                 close

main : IO ()
main = run readProject
