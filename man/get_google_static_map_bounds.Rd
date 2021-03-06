% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utility_functions.R
\name{get_google_static_map_bounds}
\alias{get_google_static_map_bounds}
\title{Coordinates of the bounding box of a static gmap}
\usage{
get_google_static_map_bounds(lat, lng, zoom, sx, sy)
}
\arguments{
\item{lat}{Center latitude}

\item{lng}{Center longitude}

\item{zoom}{Zoom Level}

\item{sx}{Width of the map}

\item{sy}{Height of the map}
}
\value{
Vector containing the bottom/left/top/right coordinates
}
\description{
The zoom level must be bigger than 8
}
\examples{



}
