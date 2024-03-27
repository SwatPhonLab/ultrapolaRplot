## Overview

The **ultrapolaRplot** library for R is designed for plotting traced ultrasound tongue imaging data according to a polar coordinate system.

There is currently support for plotting means and standard deviations of each category's trace; SSANOVA could be implemented as well.  The origin of the polar coordinates may be defined manually or automatically determined based on different algorithms.

Currently ultrapolaRplot supports ultrasound tongue imaging trace data from [UltraTrace](https://github.com/SwatPhonLab/UltraTrace).  UltraTrace is capable of importing data from Articulate Instruments AAA.


## Use

The following code snippet shows a basic example of use:

```R
library(ultrapolaRplot)

rawTraces <- loadTraces('/path/to/project/', 'vowels')  # project directory containing UltraTrace metadata file; tier to identify non-empty elements from for categories to plot
polarTraces <- makeTracesPolar(rawTraces)
plotTraces(rawTraces, polarTraces)
```

### Loading options (for `loadTraces()`)
* first argument is directory of project
* second argument (optional) is tier name to load categories from
* third argument (optional) is a list of exact categories to load (e.g., `c('nʲ', 'n')`)

### Processing options (for `makeTracesPolar()`)
* `origin.algorithm` - algorithm to use in deciding origin of polar coordinate system; either bottom middle of frames (`BottomMiddle`, default) or bottom and mean of left-right range (`BottomMean`)
* `origin.x` - override x coordinate of origin
* `origin.y` - override y coordinate of origin
* `scaling.factor` - default 800/600

### Plotting options (for `plotTraces()`)
* optional second argument - array specifying categories to plot, e.g. `c('a', 'i')`; defaults to plotting all categories in data
* `bands.fill` - boolean, whether or not to show standard deviation bands
* `bands.lines` - boolean, whether or not to show lines on edges of standard deviation bands
* `bands.linewidth` - line thickness of standard deviation bands
* `interval` - sampling interval, in degrees, for finding intersections with existing traces (default = `1`)
* `means.lines` - boolean, whether or not to display mean lines
* `means.points` - boolean, whether or not to show points on lines for means
* `means.styles` - array to override default solid line (sequentially in order of categories)
* `means.linewidth` - size of mean lines
* `standard.deviation.styles` - line type for standard deviation upper and low bands, (default = "l")
* `transparency` - transparency of standard deviation bands (default = 0.37)
* `palette` - array to override default colour palette
* `pdf.filename` - pdf file name, saves in current directory
* `png.filename` - png file name, saves in current directory
* `plot.labels` - boolean, whether or not to show labels
* `plot.ticks` - boolean, whether or not to show tick marks
* `tick.size` - size of label scaling on axises
* `points.display` - boolean, whether or not to show original annotated points
* `points.styles` - array to override default circle style (e.g., category, x, dot, etc.)
* `labels` - array to override labels
* `legend.position` - default "center", with an option of "topleft", "bottomright"
* `legend.size` - size of legend (default = 0.6)
* `legend.linewidth` - size of displayed legend lines 
* `legend` - override legend properties (position, title, border, etc.) - see legend documentation

## Installation

### Installing from CRAN

Most users will want to install ultrapolaRplot this way.  Currently the library is not in CRAN, but submission is planned.

### Installing from source

To install the latest version of ultrapolaRplot from source, download the contents of this git repository locally, and install using the following command in R, replacing `/path/to/` with the absolute or relative path (directory) to the library:

```R
devtools::install("/path/to/ultrapolaRplot/")
```

