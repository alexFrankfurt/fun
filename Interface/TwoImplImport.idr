module Interface.TwoImplImport

import Interface.Common
import Interface.Impl2
import Interface.Impl1

-- overlaping impl
-- Com where
--   str = (++ "!!!")

-- runtime error when no instance imported
main : Com => IO ()
main = do s <- getLine
          print $ str s -- uses the latest imported
