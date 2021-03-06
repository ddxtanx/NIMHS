module Nim where
import Text.Read
import Data.Char

playNormal :: IO Bool
playNormal = do
    putStrLn "Do you want to play against another player or an AI? Input 'player' or 'ai' respectively."
    st <- getLine
    let stlc = map toLower st
    if stlc /= "player" && stlc /= "ai" then do
        putStrLn "Please input either 'player' or 'ai'."
        playNormal
    else return (stlc == "player")

getNumOrStr :: String -> (Int -> Bool) -> String -> String -> String -> IO String
getNumOrStr st gi ms nne npe = do
    putStrLn ms
    n <- getLine
    let mni = readMaybe n :: Maybe Int
    case mni of
        Nothing ->
            if map toLower n /= st then do
                putStrLn nne
                getNumOrStr st gi ms nne npe
            else return n
        Just nv ->
            if gi nv then return n else do
                putStrLn npe
                getNumOrStr st gi ms nne npe

pipn = "Please input a positive number."
getStonesOrEnd :: IO String
getStonesOrEnd = getNumOrStr "end" (>0) ms ms pipn
    where
        ms = "Please input the number of stones you wish to start with or end to stop the game loop."

getInt :: Int -> Int -> String -> String -> IO Int
getInt mn mx msg errstr = do
    let mns = show mn
    let mxs = show mx
    ns <- getNumOrStr "" (\v -> v>=mn && v<=mx) msg errstr errstr
    if ns == "" then getInt mn mx msg errstr else return (read ns :: Int)

getMax :: Int -> IO Int
getMax ns = getInt 1 (ns-1) "Please input the max stones a user is allowed to take!" "Please make sure your number is more than zero and less than the number of stones."

nextPlayer :: Int -> Int
nextPlayer 1 = 2
nextPlayer 2 = 1
nextPlayer x = x

maxStones = 5

type PlayerInput = (Int -> Int -> IO Int)
gameLoopWPlayerInputs :: Int -> Int -> Int -> PlayerInput -> PlayerInput -> IO ()
gameLoopWPlayerInputs ns mxs p p1i p2i = do
    let ps = show p
    let max = min mxs ns
    let maxs = show max
    let nss = show ns
    if ns <= 0 then do
        let nps = show . nextPlayer $ p
        putStrLn ("Player " ++ nps ++ " wins!")
    else do
        n <- if p == 1 then p1i ns max else p2i ns max
        let tks = show n
        putStrLn ("Player " ++ ps ++ " took " ++ tks ++ " stones.")
        gameLoopWPlayerInputs (ns - n) mxs (nextPlayer p) p1i p2i

playerGetStones :: Int -> PlayerInput
playerGetStones p ns max = do
    let nss = show ns
    let curStonesStr = "There are currently " ++ nss ++ " stones left."
    let maxs = show max
    let ps = show p
    let playerStr = "Player " ++ ps ++ ": how many stones do you want to take? Between [1," ++ maxs ++"]"
    let errstr = "Please make sure number is in between [1," ++ maxs ++ "]."
    getInt 1 max (curStonesStr ++ "\n" ++ playerStr) errstr

perfectAI :: PlayerInput
perfectAI ns max = do
        let mp1 = max + 1
        let curMod = ns `mod` mp1
        let ts = if curMod == 0 then max else curMod
        return ts

gameLoopWPlayers :: Int -> Int -> Int -> IO ()
gameLoopWPlayers ns mxs p = gameLoopWPlayerInputs ns mxs p (playerGetStones 1) (playerGetStones 2)

gameLoopWAI :: Int -> Int -> IO ()
gameLoopWAI ns mxs = gameLoopWPlayerInputs ns mxs 1 (playerGetStones 1) perfectAI

gameLoop2Players :: Int -> Int -> IO ()
gameLoop2Players ns mxs = gameLoopWPlayers ns mxs 1
