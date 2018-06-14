# Sparsest-16-Colours
Project to find the sparsest 16 Colours in CIELUV color space.
Current version assumes sRGB.
The project consists of `SparsestColourLUV.hs` which is the finder which is written in Haskell, and some records.
  
You need the following packages: `fgl`, `colour`, `random`, and `parallel`.

Brief explanation of the algorithm:

  1. Starts with 8 colours on the vertex of the RGB color space and 8 other random colours.
  2. Makes the 2 closest colours farther to each other.
  3. Repeats 2. for certain amount, or until the current record breaks.
  4. Outputs the result colours in hexadecimal form.
  5. Outputs the minimal distance of colours **before** the last iteration of 3. (This is for simplicity of the algorithm)

In the file, you have some configurable options:

  `step`: The amount of making the colours farther in 2.
  
  `maxIter`: max iteration for 3.
  
  `record`: the minimal distance given by the previous record. **So check the previous record and change this.**
  
  `threads`: # of the threads for parallel computing.
  

See https://wiki.haskell.org/Haskell_for_multicores for enabling parallelism in Haskell.

The records must have name in the following scheme: `Sparsest16Colours_yyyymmdd.txt`.

This project was originally started for NetHack (a game). So when you find a new record and commit it here, please order the colours to vaguely fit the following order:

  0. black
  1. red
  2. green
  3. brown
  4. blue
  5. magenta
  6. cyan
  7. light gray
  8. dark gray
  9. orange
  10. bright green
  11. yellow
  12. bright blue
  13. bright magenta
  14. bright cyan
  15. white
