\name{ultrapolaRplot}
\alias{ultrapolaRplot}
\title{ultrapolaRplot}
\usage{
rawTraces <- loadTraces('/path/to/project/', 'vowels')  # project directory containing UltraTrace metadata file; tier to identify non-empty elements from for categories to plot
polarTraces <- makeTracesPolar(rawTraces)
plotTraces(rawTraces, polarTraces)
}
\description{
Loading options for loadTraces()
* first argument is directory of project
* second argument (optional) is tier name to load categories from
* third argument (optional) is a list of exact categories to load (e.g., `c('nʲ', 'n')`)

Processing options for makeTracesPolar()
* `origin.algorithm` - algorithm to use in deciding origin of polar coordinate system; either bottom middle of frames (`BottomMiddle`, default) or bottom and mean of left-right range (`BottomMean`)
* `origin.x` - override x coordinate of origin
* `scaling.factor` - default 800/600

Plotting options for plotTraces()
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
}
\examples{
library(ultrapolaRplot)
rawTraces <- loadTraces('../palatals/', 'cons')
polarTraces <- makeTracesPolar(rawTraces)
plotTraces(rawTraces, polarTraces, pdf.filename="palatals.pdf")

rawTraces <- loadTraces('../P03/', 'vowels', c('u', 'ɯ', 'o', 'ɑ', 'i', 'y', 'e', 'ø'))
polarTraces <- makeTracesPolar(rawTraces)
plotTraces(rawTraces, polarTraces, 
palette = c("#B30000", "#B30000",
"#FF69B5", "#FF69B5", "#0000FF",
"#0000FF", "#00D8FF", "#00D8FF"),
legend.position = "bottomright", points.display = FALSE, bands.lines = TRUE, bands.fill = TRUE, transparency = 0.2, mean.lines = TRUE, means.styles = c(2,1,2,1,1,2,1,2), plot.labels = TRUE, plot.ticks = TRUE, legend.size = 1, legend.linewidth = 1.5, means.linewidth = 1.5, tick.size = 1, bands.linewidth = 0.2, pdf.filename="P03.pdf")
}
