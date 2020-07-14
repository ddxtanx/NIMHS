module Main where
import Nim
main :: IO()
main = do 
    n <- getNumOrEnd
    if n=="end" then
        return ()
    else do
        let nv = read n :: Int
        normal <- playNormal
        if normal then gameLoop2Players nv else gameLoopWAI nv
        main