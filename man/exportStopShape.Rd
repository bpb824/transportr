% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gtfs-tools.R
\name{exportStopShape}
\alias{exportStopShape}
\title{Export GTFS stops to ESRI shapefile}
\usage{
exportStopShape(feedPath, outPath = NULL, shapeName = NULL,
  writeShapefile = FALSE)
}
\arguments{
\item{feedPath}{The relative or absolute path to the folder containing the GTFS feed text files}

\item{outPath}{The relative or absolute path to the folder where you would like to save the reuslting shapefile}

\item{shapeName}{The filename for your resulting shapefile}

\item{returnShape}{Boolean indicating if route shape should be written to ESRI Shapefile}
}
\value{
Returns stops shape as SpatialPointsDataFrame if desired
}
\description{
Export GTFS stops to ESRI shapefile
}

