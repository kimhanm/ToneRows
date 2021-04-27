# ToneRows
Collection of Tools to analyze serial music using tone rows


## Features
- Conversion between Classical Notation, Pitch Class Notation and Strings
```haskell
> str2Notes "DC#ABbFEbECAbGF#B"
[D,C-Sharp,A,B-Flat,F,E-Flat,E,C,A-Flat,G,F-Sharp,B]
```
- Tone Row Operations
```haskell
> transpose 5 $ inverse [2,1,9,10,5,3,4,0,8,7,5,11] 
[7,8,0,11,4,6,5,9,1,2,3,10] 
--
```


## Work in Progress

- Conversion of Pitch class set into normal form
- Symmetry group of Twelve-tone-rows under transposition, retrogrades and inverses



## Motivation
The twelve-tone row of Alban Berg's piece "Lyrische Suite" for string quartet features remarkable symmetry.

It is presented in the first movement by the first violin in the form
```haskell
F E C A G D Ab Db Eb Gb Bb B
```
When comparing the intervals between notes in the tone row we get the sequence
```haskell
[-m2, +m6, -m3, +m7, -p4, +d5, -p5, +M2, -M6, +M3, -M7]
```
one notices that every interval from minor seconds to major seventh is present, and that the reverse of the sequence is exactly the octave complement.

Such tone rows are not difficult to construct, but they form a rather small subset of the `11! = 39916800` unique tone rows.
