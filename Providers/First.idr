module Providers.First

import Effects
import Effect.File
import Effect.StdIO

%language TypeProviders

fromFile : String -> IO (Provider Type)
fromFile fname = 
  do str <- readFile fname
     let type = if trim str == "Int"
                   then Int
                   else Nat
     return (Provider type)
     
%provide (T1 : Type) with fromFile "TheType"
test : T1
test = 3     
