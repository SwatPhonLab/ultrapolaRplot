\name{differencePlot}
\alias{differencePlot}
\title{Statistical measure of differences}

\description{
  Plots the means of the differences and z-score of two segments. Allows user
  to specify origin point. 
}

\usage{
differencePlot(filteredTraces, origin.algorithm = "BottomMiddle", origin.x = NA,
scaling.factor = 800/600, x_coor = 0, y_coor = 0, interval = 1,
singleIncrements = TRUE)
}

\arguments{
  \item{filteredTraces}{dataframe of filtered segments,
  returned from plotTraces()}
  \item{origin.algorithm}{shifts x coordinates so that they are symmetrical
  around y-axis, used for plotting}
  \item{origin.x}{override x coordinate of origin.algorithm}
  \item{scaling.factor}{default 800/600}
  \item{x_coor}{used for calculating radius distances}
  \item{y_coor}{used for calculating radius distances}
}

\value{
  Returns a plot. 
}