# Drawing Voronoi Diagrams in Haskell


[Click here to go straight to the nice pictures](DIAGRAMS.md)

This is an assignment in the Functional Programming class I took at [Willamette University](https://willamette.edu/) in the Fall of 2018. Our task was to use a basic `bmp` image creator that the Professor provided and extend it to create some of our own art in Haskell! Having recently taken a Discrete and Computational Geometry course (from the wonderful [Josh Laison](https://willamette.edu/~jlaison/index.html)) and falling in love with [Voronoi Diagrams](https://en.wikipedia.org/wiki/Voronoi_diagram) I decided to try and create these in Haskell. Here are the results!

This functional programming class was taught by the wonderful (notice a trend here :) I don't think I ever had one non-wonderful Professor at Willamette) [Fritz Ruehr](http://www.willamette.edu/~fruehr/). He provided us with the `RenderBMP.hs` file and the first half of the code located in `Voronoi.hs`. My code starts after the `-- Voronoi Diagrams` comment in the `Voronoi.hs` file.

## How to draw some Voronoi Diagrams:
I generated these on my computer running Windows 10 and using the [Glasgow Haskell Compiler](https://www.haskell.org/ghc/).

1. Clone the repository onto your machine.
1. From the command prompt you can load the program using `ghci Voronoi.hs`.
1. Things are drawn by using `try <diagram>`. For example, I have defined a `voronoi` function and a Voronoi diagram can be drawn by using the command `try (voronoi 10)` (this draws a Voronoi diagram with 10 random points).
1. Look in your directory for the resulting `testy.bmp` image file (this name can be changed in the `RenderBMP.hs` file).

From here you can just look through the `Voronoi.hs` file and see the comments where I indicate how to use the function. I just tried to have some fun thinking about what I could create. There are certainly many more possibilities out there. I also left in some of Professor Ruehr's examples (`Fritz's examples`) and you can try those also: `try mickey`, `try galaxy`, etc.

*Note that drawing the Voronoi diagrams is **very** slow. Right now it is just a brute force algorithm that for each pixel calculates how far that pixel is away from each circle to determine which one is closest. Do not worry though, I did NOT get through Professor Laison's class without learning about all of the efficient algorithms for drawing Voronoi diagrams that are out there :) but to use this image creator as Professor Ruehr created it I had to use the brute force algorithm.*