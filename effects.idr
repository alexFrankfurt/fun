module Effects

import Effects
import Effect.StdIO

-- communication with program
communicate : { [STDIO] } Eff ()
communicate = do putStr "Enter: "
                 let str = !getStr
                 case str of
                   "bye" => do putStrLn "Bye!!!"
                   _     => do putStrLn str
                               communicate

main : IO ()
main = run communicate            
