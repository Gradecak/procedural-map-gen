## Haskell Procedural Map Generation

Haskell implementations of Binary Space Partition algorithm for procedural map generation

![alt text](https://github.com/gradecak/procedural-map-gen/img/raw/master/ "Sample Output")

### To run:
```bash
git clone https://github.com/Gradecak/procedural-map-gen.git && cd procedural-map-gen
stack setup
stack build
stack exec proceduralMapGen
```

There are a number of paramaters inside of the algorithm that can be tweaked to produce different results such as width/height ratios
room generation ratios and pathway widths. These can be edited directly in the BSPGen.hs file

The Implementation uses Graphics.Gloss library to display the map but it can esily be replaced out with any prefered graphics library
Rooms are defined as points x,y co-ordinates and width + height
Paths are defined as two points and a width of the path, the paths allways follow a straight line from one container to next

