\name{loadAllTracesMidPoint}
\alias{loadAllTracesMidPoint}
\title{Processes metadata and TextGrids}

\description{
  Processes layers, tiers, and categories within TextGrids. 
  Extracts x and y coordinate data from metadata.
}

\usage{
loadAllTracesMidPoint(directory_name)
}

\arguments{
  \item{directory_name}{a (readable binary-mode) \link{connection}
    or a character string giving the name of the folder 
    containing metadata and textgrid files to load
    (when \link{tilde expansion} is done).}
}

\value{
  Returns an unfiltered dataframe of filename, specific annotation number, 
  segments, x-coordinate, y-coordinate, tiers, and layer.
}