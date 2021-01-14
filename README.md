# Warhammer Fantasy Roleplay 4th edition helper
Haskell app providing the DM with some useful tools.

Currently supported features:
- Creating basic character archetypes modified with random stat rolls
- Pretty printing statlines
- Rolling dice

You can build and run the application from source code using [haskell stack](https://docs.haskellstack.org/en/stable/install_and_upgrade/).

## Examples
```
# stack ghci
> import Data.Either
> eval (fromRight (Constant 0) $ parse diceExpression "" "1d10 + 7") >>= print
9

> (($ archer human) <$> randomStats) >>= print
Entity "Human Archer" Statline { (...) } 9 []

> putStrLn $ printTable [human, elf, warrior dwarf, archer human]
| Name            | WS  | BS  | S   | T   | I   | Ag  | Dex | Int | WP  | Fel | W     |
---------------------------------------------------------------------------------------
| Human           |  30 |  30 |  30 |  30 |  30 |  30 |  30 |  30 |  30 |  30 |    12 |
| Elf             |  40 |  40 |  30 |  30 |  50 |  40 |  40 |  40 |  40 |  30 |    13 |
| Dwarf Warrior   |  50 |  30 |  40 |  50 |  30 |  20 |  40 |  30 |  50 |  20 |    16 |
| Human Archer    |  30 |  40 |  30 |  30 |  30 |  40 |  40 |  30 |  30 |  30 |    12 |
```

## Running tests
Test suite uses Hspec, so it should be installed on your system
```
stack update && stack install hspec
stack test
```
