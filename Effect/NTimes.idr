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

-- file : FileIO (OpenFile Write) ()
-- file = do True <- open "effectsInput" Write | False => putStrLn "Error!"
--           close


-- algo : Nat -> IO ()
-- algo n = do if n > 10
--               then return ()
--               else
--                 !file
--                 algo $ n + 1

hugeComputations : String
hugeComputations = "Hello, world"           
          
-- print "Hello, world!" n times
-- call it: do manyTimes n
--      or: manyTimes n                         
manyTimes : Nat -> IO ()
manyTimes n = if n == Z
                 then return ()
                 else do putStrLn "Hello, world!"
                         manyTimes (n - 1)

writeNTS : Nat -> String -> { [FILE_IO (OpenFile Write), STDIO] } Eff ()
writeNTS n s = case n of
                    Z => do return ()
                    _ => do writeLine s
                            writeNTS (n - 1) s
                  
openAndBegin : { [FILE_IO (), STDIO] } Eff ()
openAndBegin = do True <- open "outh" Write | False => putStrLn "Error!"
                  putStrLn "File opened!"
                  writeNTS 10 "jfkd"
                  close


-- writeNTS n = if n == Z
--                 then return ()
--                 else do putStrLn ("I'm writing " ++ (show n) ++ " state")
--                         writeLine (show n)
--                         writeNTS (n - 1)

-- writeMany : Nat -> { [FILE_IO (OpenFile Write), STDIO] } Eff ()
-- writeMany n = do if n == Z
--                     then return ()
--                     else do putStrLn "Hello, world!"
--                             writeMany (n - 1)                         

main : IO ()
main = run openAndBegin
