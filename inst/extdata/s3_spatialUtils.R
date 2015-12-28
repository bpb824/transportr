"IDs<-" <- function( x, value ) {
  UseMethod("IDs<-",x)
}

"IDs<-.SpatialPolygonsDataFrame" <- function( x, value) {
  spChFIDs(x,value)
}

IDs <- function(x,...) {
  UseMethod("IDs",x)
}

IDs.default <- function(x,...) {
  stop("Currently only SpatialPolygonsDataFrames are supported.")
}

IDs.SpatialPolygonsDataFrame <- function(x,...) {
  vapply(slot(x, "polygons"), function(x) slot(x, "ID"), "")
}
