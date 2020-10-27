# Warhammer Fantasy Roleplay 4th edition helper
Haskell app providing the DM with some useful tools.

Currently supported features:
- Creating basic character archetypes modified with random stat rolls
- Pretty printing statlines

You can build and run the application from source code using [haskell stack](https://docs.haskellstack.org/en/stable/install_and_upgrade/). For example
```
# stack ghci

> (eval $ fromRight (Constant 0) $ parse expression "" "1d10 + 7") >>= print
9

> randomStats >>= return . ($ (archer $ human)) >>= print
Entity "Human Archer" Statline { (...) } 9 []

> putStrLn $ printTable [human, elf, dwarf, humanArcher]
| Name            | WS  | BS  | S   | T   | I   | Ag  | Dex | Int | WP  | Fel | W     |
---------------------------------------------------------------------------------------
| Human           |  30 |  30 |  30 |  30 |  30 |  30 |  30 |  30 |  30 |  30 |    12 |
| Elf             |  40 |  40 |  30 |  30 |  50 |  40 |  40 |  40 |  40 |  30 |    13 |
| Dwarf           |  40 |  30 |  30 |  40 |  30 |  20 |  40 |  30 |  50 |  20 |    16 |
| Human Archer    |  25 |  47 |  37 |  22 |  26 |  49 |  43 |  28 |  30 |  28 |    10 |
```

Test suite uses Hspec, so it should be installed on your system
```
stack update && stack install hspec
stack test
```
