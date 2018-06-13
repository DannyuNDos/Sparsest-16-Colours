# Sparsest-16-Colours
Project to find the sparsest 16 Colours in CIELUV color space.
Current version assumes sRGB.
The project consists of a single file: SparsestColourLUV.hs, which is written in Haskell.
You need the following packages: fgl, colour, random, parallel.

Brief explanation of the algorithm:
  1. Starts with 8 colours on the vertex of the RGB color space and 8 other random colours.
  2. Makes the 2 closest colours farther to each other.
  3. Repeats 2. for certain amount, or until the current record breaks.
  4. Outputs the result colours in hexadecimal form.
  5. Outputs the minimal distance of colours **before** the last iteration of 3. (This is due to simplicity)

In the file, you have some configurable options:
  step: The amount of making the colours farther in 2.
  maxIter: max iteration for 3.
  record: the minimal distance of the previous record.
  threads: # of the threads for parallel computing.

See https://wiki.haskell.org/Haskell_for_multicores#Further_reading_9 for enabling parallelism in Haskell.
