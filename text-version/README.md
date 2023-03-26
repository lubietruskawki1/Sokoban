# Sokoban text version

| Element                | Symbol  |
| :--------------------- | ------- |
| Wall                   | #       |
| Player                 | @       |
| Box                    | $       |
| Box on a target square | *       |
| Target square          | .       |
| Empty square           | (Space) |

Game controls using the same keys as in the graphic version:

- Space - start the game
- ESC - reset
- 'U' or 'u' - undo the last move
- 'N' or 'n' - next level

Additionally:

- 'Q' or 'q' - quit the game

Player movement using arrow keys or WASD (also: wasd).

To run the game:

```shell
ghc sokoban-text-version.hs && ./sokoban-text-version
```
