module RunFile where

import qualified TreeWalk

interpFile :: String -> IO ()
interpFile filename = do
  contents <- readFile filename
  returnValue <- TreeWalk.interpString contents
  putStrLn ("exit code: " ++ show returnValue)
