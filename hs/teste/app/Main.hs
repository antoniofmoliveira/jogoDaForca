module Main where
import System.IO (hFlush)
import GHC.IO.StdHandles (stdout)

playerName :: IO String
playerName = do
    putStr "Digite o nome do jogador 2: "
    hFlush stdout
    getLine

promptLine :: String -> IO String
promptLine prompt = do
    putStr prompt
    hFlush stdout
    getLine

main :: IO ()
main = do
    player <- promptLine "Digite o nome do jogador: "
    player2 <- playerName
    putStrLn player
    