### Description

This is an R package to convert R graphics to Flash file (SWF format).
The conversion can be done in two different ways:

1. This package contains a SWF device in R, so you can create SWF file
directly using plotting functions like `plot()` and `lines()`.
The convention is that every time you call a high-level plotting function,
e.g. `plot()`, the movie will create a new frame and draw the following
shapes in this frame. In this way, you can create some animation by
calling a series of `plot()` functions.
2. This package also includes several functions to convert graphics of
other formats into SWF format. For example, `svg2swf()` to convert from
SVG to SWF, and `image2swf()` to convert PNG and JPG images into a single
SWF file.

### Installation

`R2SWF` depends on the following libraries:

- zlib     [https://www.zlib.net/](https://www.zlib.net/)
- libpng   [http://www.libpng.org/](http://www.libpng.org/)
- freetype [https://freetype.org/](https://freetype.org/)
- libming  [https://github.com/libming/libming](https://github.com/libming/libming)

The source code of `libming` is included in `R2SWF`, and you need
to install the other three by yourself.

For Debian/Ubuntu users, the command to install dependent libraries is

```
sudo apt-get install zlib1g-dev libpng12-dev libfreetype6-dev
```

For rpm based systems (e.g. Fedora), try to run

```
sudo yum install zlib-devel libpng-devel freetype-devel
```

### Examples

In the first example, we first create 20 images using `png()` function,
and then convert them into a single SWF file `R2SWF-ex1.swf`.

```r
## Creating png files
png("image-png-%03d.png", 480, 300)
x = seq(0, 2 * pi, length.out = 20)
cols = rainbow(20)
for(i in 1:20) plot(x[i], sin(x[i]), xlim = c(0, 2 * pi), ylim = c(-1, 1),
                    col = cols[i], pch = 16, cex = 2, main = "PNG => SWF")
dev.off()

## Obtain the filenames
pngfiles = sprintf("image-png-%03d.png", 1:20)

## Convert to SWF
image2swf(pngfiles, "R2SWF-ex1.swf", interval = 0.3)

```

Using `svg2swf` is pretty similar, except that the output animation contains vector graphics.

```r
## Do similar things as above
svg("image-svg-%03d.svg", 8, 5)
x = seq(0, 2 * pi, length.out = 20)
cols = rainbow(20)
for(i in 1:20) plot(x[i], sin(x[i]), xlim = c(0, 2 * pi), ylim = c(-1, 1),
                    col = cols[i], pch = 16, cex = 2, main = "SVG => SWF")
dev.off()
svgfiles = sprintf("image-svg-%03d.svg", 1:20)

## Convert to SWF
svg2swf(svgfiles, "R2SWF-ex2.swf", interval = 0.3)

```

The third example shows how to use the SWF device to create (rather than converting) SWF file directly.

```r
swf("R2SWF-ex3.swf")
set.seed(123)
x = rnorm(5)
y = rnorm(5)
for(i in 1:100) {
    plot(x <- x + 0.1 * rnorm(5), y <- y + 0.1 * rnorm(5),
         xlim = c(-3, 3), ylim = c(-3, 3), col = "steelblue",
         pch = 16, cex = 2, xlab = "x", ylab = "y")
    title("Brownian Motion")
}
dev.off()
```

In general, when using the SWF device, high-level plotting functions (e.g. `plot()`)
will advance the movie by one frame, and low-level functions (`lines()`, `text()`, etc.)
are effective only to the current frame.
