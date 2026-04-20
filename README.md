## Overview

The **ultrapolaRplot** library for R is designed for plotting traced ultrasound tongue imaging data according to a polar coordinate system.

There is currently support for plotting means and standard deviations of each category's trace; SSANOVA could be implemented as well.  The origin of the polar coordinates may be defined manually or automatically determined based on different algorithms.

Points for each category can be split into two groups (anterior and posterior) at the point of maximum curvature (PMC) of each trace. User can specify rays to intersect various parts of the tongue; intersections along these rays serve as input for a pairwise t-test to measure significant contrasts between segments. 

Currently ultrapolaRplot supports ultrasound tongue imaging trace data from [UltraTrace](https://github.com/SwatPhonLab/UltraTrace).  UltraTrace is capable of importing data from Articulate Instruments AAA.

**See [publications](https://github.com/SwatPhonLab/ultrapolaRplot-pubs) related to ultrapolaRplot**, currently our Ultrafest poster and extended abstract.  Be sure to cite them if you use ultrapolaRplot!


## Use

The following code snippet shows a basic example of use:

```R
library(ultrapolaRplot)

rawTraces <- loadAllTracesMidPoint(filepath)  # project directory containing UltraTrace metadata file; tier to identify non-empty elements from for categories to plot
filteredTraces <- plotTraces(rawTraces, categoriesAll = c("o", "i"), bestFitRays = TRUE, perpendicularRays = TRUE, bestFitRays.intersection_rays.positive = c(0.5), difference_plot = TRUE)
```

### Loading options (for `loadAllTracesMidPoint()`)
* first argument is directory of project

### Plotting options (for `plotTraces()`)
* `rawTraces` - data frame returned from loadAllTracesMidPoint()
* `polarTraces` - returned from makeTracesPolar(), optional, outdated
* `layersAll` - list of layers within metadata to extract x and y coodinate data from. Defaults to 'tongue' layer.
* `tiernameAll` - respective tiers (if applicable) within layers. If none specified, all tiers are checked.
* `categoriesAll` - respective categories of segments to extract within tiers.
* `mergeCategories` - boolean or boolean array, as to whether to merge respective categories.
* `seg_filter` - respective segment_text for additional filtering
* `origin.algorithm` - algorithm to use in deciding origin of polar coordinate system; either bottom middle of frames (`BottomMiddle`, default) or bottom and mean of left-right range (`BottomMean`)
* `origin.x` - override x coordinate of origin
* `origin.y` - override y coordinate of origin
* `scaling.factor` - default 800/600
* `angle_between_best_fit` - boolean, whether or not to display violin plot of angles between the bestfit lines per category
* `bands.fill` - boolean, whether or not to show standard deviation bands
* `bands.lines` - boolean, whether or not to show lines on edges of standard deviation bands
* `bands.linewidth` - line thickness of standard deviation bands
* `bestFitRays` - boolean, whether or not to display lines of best fit for tongue body and tongue root
* `bestFitRays.start_point_density` - Number of intersecting traces per category needed to extend bestFitRays until (default = 1), 0 would be until farthest points, 2 would be until each segment has standard deviation
* `bestFitRays.intersection_rays.negative` - List of percentages along bestfit ray on left side to intersect data
* `bestFitRays.intersection_rays.positive` - List of percentages along bestfit ray on right side to intersect data
* `ray_color` - color of bestFitRays.intersection_rays.negative and bestFitRays.intersection_rays.positive (default = "darkgrey")
* `angle_neg_rotate` - Corresponding angles to bestFitRays.intersection_rays.negative (default = 0) to rotate rays while still keeping the intersections with the bestfit line the same
* `angle_pos_rotate` - Corresponding angles to bestFitRays.intersection_rays.positive (default = 0) to rotate rays while still keeping the intersections with the bestfit line the same
* `rays` - list of arrays to specify (x,y), and angle of intersecting ray
* `bestFitRays.show_elbows` - boolean, whether or not to show PMC along means
* `bubble` - boolean, whether or not to display PMC cluster graph and pairwise comparison
* `difference_plot` - boolean, whether or not to display difference_plot for the first two segments
* `elbow_color` - color of PMC along means (default = "black")
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

Most users will want to install ultrapolaRplot this way.

```R
install.packages ('ultrapolaRplot')
```

### Installing from source

Most users will want to install ultrapolaRplot fromm CRAN, as above.  Installing from source is primarily useful for developers, or if you want the latest (unsupported) version of the code.

To install the development version of ultrapolaRplot from source, download the contents of this git repository locally, and install using the following command in R, replacing `/path/to/` with the absolute or relative path (directory) to the library:

```R
devtools::install("/path/to/ultrapolaRplot/")
```
