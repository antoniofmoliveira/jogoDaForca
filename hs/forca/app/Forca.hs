module Forca
  ( play
  ) where

import           Control.Monad   ()
import           Data.Char       (toLower)
import           Data.List.Split (splitOn)
import           Data.Map        (Map, foldrWithKey, fromList, insert, lookup)
import           System.IO       (hFlush, stdout)
import           System.Random   (Random (randomR), getStdRandom)
import           Text.Printf     (printf)

{- | Retorna o patíbulo com as partes do enforcado conforme o número de erros. -}
getGallows :: Int -> [Char]
getGallows numErrors = do
  let topGallow = "X==:==\nX  :\n"
      bottomGallow = "X\n===========\n"
      gallow =
        [ "X\nX\nX\n"
        , "X  O\nX\nX\n"
        , "X  O\nX  |\nX\n"
        , "X  O\nX /|\nX\n"
        , "X  O\nX /|\\\nX\n"
        , "X  O\nX /|\\\nX / \n"
        , "X  O\nX /|\\\nX / \\\n"
        ]
  topGallow ++ (gallow !! numErrors) ++ bottomGallow

{- | Carrega um arquivo de texto retornando uma lista com as palavras do arquivo. -}
loadFile :: FilePath -> IO [String]
loadFile filePath = do
  contents <- readFile filePath
  return $ words contents

{- | Grava um arquivo de texto. -}
saveFile :: FilePath -> String -> IO ()
saveFile texto filePath = do
  writeFile texto filePath

{- | Carrega um arquivo com os recordes registrados no jogo. -}
loadRecords :: FilePath -> IO (Map [Char] [Char])
loadRecords filePath = do
  content <- readFile filePath
  return
    $ fromList
    $ Prelude.map ((\[e1, e2] -> (e1, e2)) . splitOn ";") (lines content)

{- | Grava um arquivo com os recordes registrados no jogo. -}
saveRecords :: Map [Char] [Char] -> FilePath -> IO ()
saveRecords records filePath = do
  let lines = foldrWithKey (\k v ks -> ks ++ k ++ ";" ++ v ++ "\n") [] records
  writeFile filePath lines

playerName :: IO String
{- | Solicita e retorna o nome do jogador. -}
playerName = do
  putStr "Digite o nome do jogador: "
  hFlush stdout -- necessário para a mensagem ser exibida antes do getline. faz o flush do buffer de saída.
  >> getLine

{- | Solicita um caractere e compara com os já digitados. Repete se já digitou o caractere. -}
getAChar :: [Char] -> IO [Char]
getAChar chars = do
  putStr "\nDigite uma letra:"
  hFlush stdout -- necessário para a mensagem ser exibida antes do getline. faz o flush do buffer de saída.
  line <- getLine
  let char = toLower $ head line
  if char `elem` chars
    then do
      putStrLn "Você já tentou esta letra!"
      getAChar chars
    else return [char]

{- | Compara os caracteres digitados com a palavra secreta.
     Devolve a palavra secreta com os caracteres não digitados mascarados com '.'. -}
checkWord :: [Char] -> [Char] -> [Char]
checkWord _ [] = []
checkWord [] word = replicate (length word) '.'
checkWord chars (x:xs)
  | x `elem` chars = x : checkWord chars xs
  | otherwise      = '.' : checkWord chars xs

{- | Recebe a lista de palavras, sorteia e retorna a palavra secreta do jogo.-}
selectWord :: [[Char]] -> IO [Char]
selectWord texto = do
  ind <- getStdRandom $ randomR (0, length texto - 1)
  return $ texto !! ind

{- | Atualiza Data.Map com os recordes do jogo. -}
updateRecords ::
     Map [Char] [Char] -> [Char] -> [Char] -> Maybe [Char] -> Map [Char] [Char]
updateRecords records "" _ _ = records
updateRecords records name numErros Nothing = insert name numErros records
updateRecords records name numErros (Just previousNumErrors) =
  if previousNumErrors > numErros
    then insert name numErros records
    else records

{- | Função auxiliar para listRecords usada para juntar keys e values do Data.Map
    de recordes com a função Data.Map.forldrWithKey. -}
f key value result = result ++ key ++ " " ++ value ++ "\n"

{- | Exibe os recordistas do jogo. -}
listRecords :: [Char] -> Int -> IO ()
listRecords name numErrors = do
  records <- loadRecords "forcarecordes.txt"
  let previousNumErrors = Data.Map.lookup name records
      recordsf = updateRecords records name (show numErrors) previousNumErrors
  putStrLn "\nRecordistas"
  putStrLn "============"
  putStrLn $ Data.Map.foldrWithKey f "" recordsf

{- | Rotina principal do jogo. Recursivo. Recebe a lista dos caracteres digitados, a
    palavra secreta, o número de erros e o nome do jogador.
    Solicita caracter. Confere se acertou a palavra.
    Se atingiu o numero maximo de erros e exibe mensagem, exibe recordes e encerra.
    Se caracter digitado pertence à palavra secreta mas não a completou
    atualiza a lista de caracteres e chama recursivamente a função sem aumentar o número de erros.
    Se caracter digitado pertence à palavra secreta e a completou
    exibe mensagem e imprime recordes.
    Se caracter digitado não pertence à palavra secreta atualiza a lista de caracteres e
    chama recursivamente a função aumentando em 1 o número de erros.-}
game :: [Char] -> [Char] -> Int -> [Char] -> IO ()
game chars secretWord numErrors player = do
  putStrLn $ getGallows numErrors
  if numErrors < 6
    then do
      theChar <- getAChar chars
      let wordBefore = checkWord chars secretWord
          wordAfter = checkWord (chars ++ theChar) secretWord
      putStrLn ("'" ++ wordAfter ++ "'")
      if wordAfter /= wordBefore
        then do
          if wordAfter == secretWord
            then do
              putStrLn "Você acertou!!"
              listRecords player numErrors
            else game (chars ++ theChar) secretWord numErrors player
        else do
          game (chars ++ theChar) secretWord (numErrors + 1) player
    else do
      putStrLn "Você perdeu!"
      listRecords "" numErrors

{- | Inicialização do jogo.
    Carrega a lista de palavras e solicita o nome do jogador.
    Inicia o fluxo do jogo com uma lista vazia de caracteres digitados, a palavra secreta
    sorteada, número de erros zerado e o nome do jogador.-}
play :: IO ()
play = do
  texto <- loadFile "palavras.txt"
  name <- playerName
  sel <- selectWord texto
  game "" sel 0 "name"
