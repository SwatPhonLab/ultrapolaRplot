## Use

```R
library(ultrasoundPlotting)

rawTraces <- loadTraces('/path/to/project/', 'vowels')  # project directory containing UltraTrace metadata file; tier to identify non-empty elements from for categories to plot
polarTraces <- makeTracesPolar(rawTraces)
plotTraces(rawTraces, polarTraces)
```

### Processing options (for `makeTracesPolar()`)
* `origin.algorithm` - algorithm to use in deciding origin of polar coordinate system; either bottom middle of frames (`BottomMiddle`, default) or bottom and mean of left-right range (`BottomMean`)
* `origin.x` - override x coordinate of origin
* `origin.y` - override y coordinate of origin
* `scaling.factor` - default 800/600

### Plotting options (for `plotTraces()`)
* optional second argument - array specifying categories to plot, e.g. `c('a', 'i')`; defaults to plotting all categories in data
* `bands.fill` - boolean, whether or not to show standard deviation bands
* `bands.lines` - boolean, whether or not to show lines on edges of standard deviation bands
* `interval` - sampling interval, in degrees, for finding intersections with existing traces (default = `1`)
* `means.points` - boolean, whether or not to show points on lines for means
* `means.styles` - array to override default solid line (sequentially in order of categories)
* `standard.deviation.styles` - line type for standard deviation upper and low bands
* `points.display` - boolean, whether or not to show original annotated points
* `points.styles` - array to override default circle style (e.g., category, x, dot, etc.)
* `myPalette` - array to override default colour palette
* `labels` - array to override labels
* `legend.position` - default "center", with an option of "topleft"
* `legend` - override legend properties (position, title, border, etc.) - see legend documentation
