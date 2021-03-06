% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/geo_utilities.R
\name{assign_polygon_index}
\alias{assign_polygon_index}
\title{Assigns the polygon index}
\usage{
assign_polygon_index(df_polygons, df_sp)
}
\arguments{
\item{df_polygons}{a sf data frame containing the polygons}

\item{df_sp}{a sf data frame containing the points to label}
}
\value{
a new dataframe with the column index_of_containing_polygon filled
}
\description{
Assigns the polygon index
}
