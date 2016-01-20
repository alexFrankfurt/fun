module Ibt.Main

import Effects
import Effect.StdIO
import Effect.File

loadingMessage : String
loadingMessage = "Loading project definition..."

buildFileName : String
buildFileName = "build.ibt"

noBuildFileErrorMessage : String
noBuildFileErrorMessage = "Error."

readProject : { [FILE_IO (), STDIO] } Eff ()
readProject = do putStrLn loadingMessage
                 True <- open buildFileName ReadWrite
                 | False => putStrLn noBuildFileErrorMessage
                 close

main : IO ()
main = run readProject
