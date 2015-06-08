module NTimes

import Effects
import Effect.StdIO
import Effect.File

hello : Eff () [STDIO] 
hello = do putStr "Enter your name "
           x <- getStr
           putStrLn ("Hello, " ++ trim x ++ "!")

FileIO : Type -> Type -> Type
FileIO st t = Eff t [FILE_IO st, STDIO]

file : FileIO (OpenFile Write) ()
file = do True <- open "effectsInput" Write | False => putStrLn "Error!"
          close


algo : Nat -> IO ()
algo n = do if n > 10
              then return ()
              else
                !file
                algo $ n + 1
          

main : IO ()
main = run (algo 0)
