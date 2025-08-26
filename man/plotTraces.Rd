\name{plotTraces}
\alias{plotTraces}
\title{Plots ultrasound polar coordinate data}

\description{
  Plots extracted segments with means and standard deviation bands calculated
  through use of polar coordinates.
}

\usage{
plotTraces(rawTraces, polarTraces, interval = 1, mean.lines =
                 TRUE, points.display = FALSE, palette = c(),
                 bands.lines = FALSE, bands.fill = TRUE,
                 legend.position = "topleft", means.styles = c(),
                 standard.deviation.styles = "l", plot.ticks = FALSE,
                 plot.labels = FALSE, legend.size = 3, transparency =
                 0.37, pdf.filename = c(), bands.linewidth = 0.3,
                 png.filename = c(), legend.linewidth = 5,
                 means.linewidth = 3, tick.size = 2, maskCategories =
                 c(), rays = list(c(x_coor = 0, y_coor = 0, angle = 1.57)))
}

\arguments{
  \item{rawTraces}{data frame returned from loadTraces()}
  \item{polarTraces}{returned from makeTracesPolar()}
  \item{bands.fill}{boolean, whether or not to show standard deviation bands}
  \item{bands.lines}{boolean, whether or not to show lines on edges of standard
  deviation bands}
  \item{bands.linewidth}{line thickness of standard deviation bands}
  \item{interval}{sampling interval, in degrees, for finding intersections with
  existing traces (default = `1`)}
  \item{mean.lines}{boolean, whether or not to display mean lines}
  \item{means.styles}{array to override default solid line (sequentially in 
  order of categories)}
  \item{means.linewidth}{size of mean lines}
  \item{standard.deviation.styles}{line type for standard deviation upper and 
  low bands, (default = "l")}
  \item{transparency}{transparency of standard deviation bands 
  (default = 0.37)}
  \item{palette}{array to override default colour palette}
  \item{pdf.filename}{pdf file name, saves in current directory}
  \item{png.filename}{png file name, saves in current directory}
  \item{plot.labels}{boolean, whether or not to show labels}
  \item{plot.ticks}{boolean, whether or not to show tick marks}
  \item{tick.size}{size of label scaling on axises}
  \item{points.display}{boolean, whether or not to show original 
  annotated points}
  \item{labels}{array to override labels}
  \item{legend.position}{default "center", with an option of
  "topleft", "bottomright"}
  \item{legend.size}{size of legend (default = 0.6)}
  \item{legend.linewidth}{size of displayed legend lines }
  \item{maskCategories}{array to override current segment labels}
  \item{rays}{list of rays, each specified by its x_coor, y_coor, and angle}
}
\value{
  Returns a plot. User can additionally export plot to a pdf or png.
}