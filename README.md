# LL diagram generator

try:
```
cabal run ll-diagram-generator -- --scale 2 --right magenta "F R U R' U' F'" > test.svg
```
(you might have to do it twice because cabal/ghc likes to write extra stuff to stdout the first time you compile)

run `cabal run ll-diagram-generator -- --help` to see all options

**things i want to add**:

- OLL and OLLCP modes that grey out certain stickers (can already do OLL mode manually with colour flags but it's a hassle)
- more powerful alg parsing
    - modifiers on parenthesised groups e.g. `F (R U R' U')3 F'`
    - conjugate/commutator notation e.g. `[l': [R' D2 R, U]]`
    - expand triggers e.g. `F sexy F'`
    - notation for mirroring things to the left/back e.g. `back sune`
- more styling choices perhaps (gap width, outline colour, background...)
- full cube net mode (might have to change the project name though since that wouldn't be last layer diagrams)
- tests probably i guess
