module Main

import Effects
import Effect.File
import Effect.StdIO
import Effect.State

FileIO : Type -> Type -> Type
FileIO st t = Eff t [FILE_IO st, STDIO]

fileWork : FileIO () ()
fileWork = do True <- open "effectsInput" Write | False => putStrLn "Error!"
              close

counter : Eff () [STATE Int, STDIO]
counter = do putStrLn $ "Counter at " ++ show !get
             x <- getStr
             if trim x /= "" then pure ()
                             else do put (!get + 1)
                                     counter

startCounter : Eff () [STDIO]
startCounter = do putStrLn "Off we go!"
                  new (STATE Int) 0 counter
                  putStrLn "Finished!"

main : IO ()
main = run startCounter
