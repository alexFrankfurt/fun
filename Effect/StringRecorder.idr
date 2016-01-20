module StringRecorder

import Effects
import Effect.State
import Effect.StdIO
import Data.Vect

rec : TransEff.Eff ()
                   [STATE (Vect n String), STDIO]
                   [STATE (Vect (S n) String), STDIO]
rec = do putStrLn "Enter text: "
         x <- getStr
         --put (x :: !get)
         rec

-- main : IO ()
-- main = runInit [Data.Vect.Nil] rec
