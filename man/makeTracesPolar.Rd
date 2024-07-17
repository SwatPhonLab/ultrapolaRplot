\name{makeTracesPolar}
\alias{makeTracesPolar}
\title{Converts to polar coordinates}

\description{
  Converts raw coordinate data from loadTraces into polar coordinates. 
  Options available to specify the x-coodrindate of origin tranducer, including
  algorithms for BottomMean and BottomMiddle. Sample interval degrees of ray 
  can be specified as well.
}

\usage{
makeTracesPolar(rawTraces, origin.algorithm = "BottomMiddle", origin.x = NA, 
scaling.factor = 800/600)
}

\arguments{
  \item{rawTraces}{data frame returned from loadTraces()}
  \item{origin.algorithm}{takes list of all extracted x-coordinates and sets}
  \item{origin.x}{override x coordinate of origin}
  \item{scaling.factor}{default 800/600}
}

\value{
  Returns a list of matrices. Each item within the list is labelled by its 
  respective segment, such as 'e', 'i' and 'o'. Each segment is respectively 
  further labeled by the trace number such as 'e[[1]]' and for example up to
  'e[[11]]', if there are 11 traces for segment 'e'.
  There is a matrix for each trace within a segment. The column number 
  represents the individual point along that trace, and the row values 
  contain 1) x-coordinate, 2) y-coordinate, 3) angle in radians from
  origin (0,0), and 4) radius, for that point. 

}