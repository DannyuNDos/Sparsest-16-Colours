# Sparsest-16-Colours
Project to find the sparsest 16 Colours in CIELUV color space.

Current version assumes sRGB. The project consists of:

    1. SparsestColourLUV.hs, old version of finder, written in Haskell. This is to be used for finding colours in vague sense.
    2. SparsestColourLUV_Discrete.hs, new version of finder as of 2018.10.29, also written in Haskell. This is to be used to refine results from 1, though it can be used standalone.
    3. Some records.

You need the following Haskell packages: `fgl`, `colour`, and `random`.

Brief explanation of SparsestColourLUV.hs:

    1. Starts with 8 colours on the vertex of the RGB color space and 8 other random colours.
    2. Makes the 2 closest colours farther to each other.
    3. Repeats Step 2 for certain amount, or until the current record is broken.
    4. Outputs the result colours in hexadecimal form.
    5. Outputs the minimal distance of colours (a.k.a. MD) after the last iteration of Step 3.

In this file, you have some configurable options:

`step`: The amount of making the colours farther in Step 2.

`maxIter`: Max iteration for Step 3.

`record`: The minimal distance given by the previous record. **So check the previous record and change this.**

`threads`: # of the threads for parallel computing. This makes execution of the same procedure in parallel, and will output only the best result.

`outputPath`: Path for outputting the result. This is `"./Sparsest16Colours.txt"` by default.

Brief explanation of SparsestColourLUV_Discrete.hs:

    1. Starts with given colours in outputPath.
    2. Select a random colour amongst them, then moves it to most appropriate adjacent colour.
    3. Repeats Step 2 for certain amount, or until the current record is broken.
    4. Outputs the result colours in hexadecimal form.
    5. Outputs the square of minimal distance of colours (a.k.a. SMD) after the last iteration of Step 3.
    6. Also outputs the sum of square of distances of colours from average colour (a.k.a. SSDA).

In this file, the configurable options are:

`coloursN`: The number of desired colours.

`maxIter`: Max iteration for Step 3.

`outputPath`: Path for outputting the result. This is `"./Sparsest" ++ show coloursN ++ "Colours.txt"` by default.

Due to different syntax of outputs of both versions, if you used the old version and want to use its result to the new version, the syntax of the record must be adjusted. Namely, the "Minimal Distance: " must be erased, the MD itselt must be squared, and dummy SSDA must be inserted as 0. (This issue is to be fixed later.)

All records must have file name in the following scheme: `Sparsest16Colours_yyyymmdd.txt`.

This project was originally started for NetHack (a game). So if you are finding for 16 colours, when you find a new record and commit it here, please order the colours to vaguely fit the following order:

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
