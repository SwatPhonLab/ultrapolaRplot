\name{loadTraces}
\alias{loadTraces}
\title{Processes metadata and TextGrids}

\description{
  Processes layers, tiers, and categories within TextGrids. 
  Extracts x and y coordinate data from metadata.
}

\usage{
loadTraces(directory_name, tiernameAll = c(""), categoriesAll = list(c()),
layersAll = c(""), mergeCategories = c(FALSE))
}

\arguments{
  \item{directory_name}{a (readable binary-mode) \link{connection}
    or a character string giving the name of the folder 
    containing metadata and textgrid files to load
    (when \link{tilde expansion} is done).}
  \item{tiernameAll}{respective tiers (if applicable) within layers. If none
  specified, all tiers are checked. Accepts either string or list of respective
  tiers.}
  \item{categoriesAll}{respective categories of segments to extract within 
  tiers. If respective tiername and categories specified as "" and c(""), 
  textgrids will not be checked and coordinates will be extracted from metadata
  without labeling segment. If all categories specified through c(), all 
  labeled segments from respective layers and tiers will be extracted.}
  \item{layersAll}{list of layers within metadata to extract 
  x and y coodinate data from. Defaults to 'tongue' layer. Accepts either 
  string or list of layers.}
  \item{mergeCategories}{boolean or boolean array, as to whether to merge 
  respective categories.}
}

\value{
  Returns dataframe of filename, specific annotation number, segment,
  x-coordinate, y-coordinate.
}