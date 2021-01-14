module Language.Repl where

import Language.Main
import Language.Dice
import Language.Entity

quitString :: Parser String
quitString = try $ choice $ map string ["quit", "exit"]

data TopLevelExpression = TlDice DiceExpression | TlEntity EntityExpression | TlQuit QuitExpression
newtype QuitExpression = QuitExpression String

topLevelP :: Parser TopLevelExpression
topLevelP = choice [ 
    TlQuit . QuitExpression <$> quitString
    , TlEntity <$> entityExpression 
    , TlDice <$> diceExpression
    ]

parseTopLvl :: String -> Either ParseError TopLevelExpression
parseTopLvl = parse topLevelP ""