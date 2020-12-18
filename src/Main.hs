module Main where

import Language.Main
import Language.Repl
import Language.Dice
import System.IO

main :: IO ()
main = parseLine

parseLine :: IO ()
parseLine = do
  putStr "> " 
  hFlush stdout
  str <- getLine
  result <- showResult $ parseTopLvl str
  printOrQuit result

showResult :: Either ParseError TopLevelExpression -> IO (Maybe String)
showResult res = case res of
  Left err -> return $ Just $ show err
  Right topLvl -> case topLvl of
    TlQuit _ -> return Nothing
    TlDice d -> Just . show <$> eval d
    TlEntity e -> return $ Just $ show e

printOrQuit :: Maybe String -> IO ()
printOrQuit (Just s) = putStrLn s >> parseLine
printOrQuit Nothing = return ()



