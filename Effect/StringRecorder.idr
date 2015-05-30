module StringRecorder

import Effects
import Effect.State
import Effect.StdIO

rec : { [STATE (Vect n String), STDIO] ==> 
        [STATE (Vect (S n) String), STDIO] } Eff ()
rec = do putStrLn "Enter text: "
         x <- !getLine
         put (x :: !get)
         rec

main : IO ()
main = runInit rec
