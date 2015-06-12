module File

import Effects
import Effect.File
import Effect.State
import Effect.StdIO
import Control.IOExcept

data Count n

FileIO : Type -> Type -> Type
FileIO st t = Eff t [FILE_IO st, STDIO, Count ::: STATE Int]

readFile : FileIO (OpenFile Read) (List String)
readFile = readAcc [] where
    readAcc : List String -> FileIO (OpenFile Read) (List String)
    readAcc acc = do e <- eof
                     if (not e)
                        then do str <- readLine
                                Count :- put (!(Count :- get) + 1)
                                readAcc (str :: acc)
                        else return (reverse acc)

testFile : FileIO () ()
testFile = do True <- open "effectsInput" Read | False => putStrLn "Error!"
              putStrLn (show !readFile)
              close
              putStrLn (show !(Count :- get))

main : IO ()
main = run testFile 
