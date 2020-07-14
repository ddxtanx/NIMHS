module Main where
import Nim
main :: IO()
main = do 
    n <- getStonesOrEnd
    if n=="end" then
        return ()
    else do
        let nv = read n :: Int
        mxstones <- getMax nv
        normal <- playNormal
        if normal then gameLoop2Players nv mxstones else gameLoopWAI nv mxstones
        main