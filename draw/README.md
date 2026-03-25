# draw

Richard Wen  
rrwen.dev@gmail.com  
  
User-friendly functions for creating consistent and reproducible graphics and diagrams with lines, common shapes, and text. Compatible with and based on the R 'grid' package.


## Install

1. Install [R](https://www.r-project.org/)
2. Install the [RStudio](https://www.rstudio.com/products/rstudio/download/#download) code editor
3. Open an [R Console](https://support.rstudio.com/hc/en-us/articles/200404846-Working-in-the-Console) in RStudio
3. Install [draw](https://github.com/rrwen/draw) in an R console with [install.packages](https://www.rdocumentation.org/packages/utils/versions/3.5.1/topics/install.packages)

```R
install.packages("draw")
```

## Usage

1. Load [draw](https://github.com/rrwen/draw) with [library](https://www.rdocumentation.org/packages/base/versions/3.5.1/topics/library)
2. Set page dimensions and units with `drawSettings`
3. Create a new page with `drawPage`
4. Draw graphics with `drawBox`, `drawCircle`, `drawLine`, `drawText`
5. Export graphics to a file with `drawExport`

```R
library(draw)

# Set drawing settings
drawSettings(pageWidth = 5, pageHeight = 5, units = "inches")

# Create a new drawing page
drawPage()

# Draw graphics on the page
drawBox(x = 2.5, y = 2.5, width = 1, height = 1)
drawCircle(x = 2.5, y = 2.5, radius = 0.5)
drawLine(x = c(1, 4),
         y = c(1 ,1))
drawText(x = 2.5, y = 2.5, text = "TEXT")

# Export the drawing page to a PDF
drawExport("draw.pdf")

# Export the drawing page to a JPEG
drawExport("draw.jpeg", ppi = 300)
```
