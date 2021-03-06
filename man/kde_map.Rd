% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functions.R
\name{kde_map}
\alias{kde_map}
\title{kde_map}
\usage{
kde_map(sp, perc = 95, units = "km2")
}
\arguments{
\item{sp}{A SpatialPointsDataFrame {sp} containing track (relocation) points of groups or individuals. It should contain 3 columns: "id", "x", "y", where x and y are UTM point coordinates.}

\item{perc}{Percentage of estimated distribution to include in polygons. Default is 95.}

\item{units}{Units to output area. Takes "km2", "m2", "ha" for hectares. Default is "km2".}
}
\description{
Maps polygons generated using kernel density estimates for one or more groups.
}
\examples{
kde_map(sp = "trackpoints", perc = 90, units = "m2")
}
\keyword{KDE}
\keyword{kernel}
\keyword{map}
