#' Fetch GTFS feed files
#'
#' This function will match the requested transit agency name with a feed via the GTFS data exchange, and the latest feed files will be downloaded to 'feeds' subfolder of working directory.
#'
#' @param feedName Name of transit agency (will try to find similar names if not exact match)
#'
#' @return None
#' @export
fetchFeed = function(feedName){
  url = "http://www.gtfs-data-exchange.com/api/agencies"
  gtfsFeeds = httr::content(httr::GET(url),as = "parsed", type ="application/json")
  feedData = transportr::list2frame(gtfsFeeds$data)
  row = grep(tolower(feedName), tolower(feedData$name))
  if(length(row)==0){
    stop("Could not find the feed you are looking for. The available feeds are listed here: http://www.gtfs-data-exchange.com/agencies")
  }else if(length(row)==1){
    feedInfo = feedData[row,]
    fileURL = paste0(feedInfo$dataexchange_url,"latest.zip")
    if(!dir.exists("feeds")){
      dir.create("feeds")
    }
    if(!dir.exists(paste0("feeds/",feedInfo$name))){
      dir.create(paste0("feeds/",feedInfo$name))
    }
    download.file(fileURL,paste0("feeds/",feedInfo$name,"/feed.zip"))
    unzip(paste0("feeds/",feedInfo$name,"/feed.zip"), exdir=paste0("feeds/",feedInfo$name))
    print(paste0("Feed downloaded and unzipped to ","feeds/",feedInfo$name))
  }else{
    print("More than one result available. Here are the names of the transit agencies found from your search term:")
    feedData$name[row]
  }
}

#' Create SpatialLines from GTFS shapes.txt file
#'
#' @param feedPath Absolute or relative path to the folder containing GTFS feed files.
#'
#' @return SpatialLines object corresponding to shapes.txt file
#' @export
spatialTransitShapes = function(feedPath){
  shapes = read.csv(paste0(feedPath,"/shapes.txt"), stringsAsFactors = FALSE)
  shapeCoords = list()
  shapeList = unique(shapes$shape_id)
  for (i in 1:length(shapeList)){
    shapeCoords[[as.character(shapeList[i])]]= shapes[shapes$shape_id == shapeList[i],]
  }
  lineList = list()
  for (i in 1:length(shapeCoords)){
    id = names(shapeCoords[i])
    shape = sp::Line(shapeCoords[[i]][,c("shape_pt_lon",c("shape_pt_lat"))])
    lineList[[i]]= sp::Lines(list(sp::Line(shape)),id)
  }
  transitShapes = sp::SpatialLines(lineList, sp::CRS("+init=epsg:4326"))
  return(transitShapes)
}

#' Export GTFS routes to ESRI shapefile
#'
#' @param feedPath The relative or absolute path to the folder containing the GTFS feed text files
#' @param outPath The relative or absolute path to the folder where you would like to save the reuslting shapefile
#' @param shapeName The filename for your resulting shapefile
#' @param returnShape Boolean indicating if route shape should be returned
#'
#' @return Returns route shape as SpatialLinesDataFrame if desired
#' @export
exportRouteShape = function(feedPath,outPath,shapeName,returnShape=FALSE){
  shapes = read.csv(paste0(feedPath,"/shapes.txt"),stringsAsFactors = FALSE)
  routes = read.csv(paste0(feedPath,"/routes.txt"),stringsAsFactors = FALSE)
  trips = read.csv(paste0(feedPath,"/trips.txt"),stringsAsFactors = FALSE)
  #gtfs_stops = read.csv(paste0(feedPath,"/stops.txt"),stringsAsFactors = FALSE)
  
  shapeCoords = list()
  shapeList = sort(unique(trips$shape_id))
  for (i in 1:length(shapeList)){
    shapeCoords[[as.character(shapeList[i])]]= shapes[shapes$shape_id == shapeList[i],]
  }
  lineList = list()
  for (i in 1:length(shapeCoords)){
    id = names(shapeCoords[i])
    shape = sp::Line(as.data.frame(shapeCoords[[i]])[,c("shape_pt_lon",c("shape_pt_lat"))])
    lineList[[i]]= sp::Lines(list(sp::Line(shape)),id)
  }
  transitShapes = sp::SpatialLines(lineList, sp::CRS("+init=epsg:4326"))
  
  shapeIds = sort(unique(trips$shape_id))
  shapeData = data.frame(matrix(nrow = length(shapeIds),ncol =7))
  colnames(shapeData)=c("shape_id","route_id","direction_id","route_short_name","route_long_name","route_type","route_url")
  shapeData$shape_id = shapeIds
  for (i in 1:nrow(shapeData)){
    sid = shapeData$shape_id[i]
    shapeData$route_id[i] = trips$route_id[trips$shape_id==sid][1]
    shapeData$direction_id[i] = trips$direction_id[trips$shape_id==sid][1]
    shapeData$route_short_name[i]=routes$route_short_name[routes$route_id==shapeData$route_id[i]]
    shapeData$route_long_name[i]=routes$route_long_name[routes$route_id==shapeData$route_id[i]]
    shapeData$route_type[i]=routes$route_type[routes$route_id==shapeData$route_id[i]]
    shapeData$route_url[i]=routes$route_url[routes$route_id==shapeData$route_id[i]]
    #print(i)
  }
  rownames(shapeData)=shapeData$shape_id
  
  route_shape = sp::SpatialLinesDataFrame(transitShapes,shapeData)
  
  rgdal::writeOGR(route_shape,outPath,shapeName,overwrite_layer = TRUE,driver = "ESRI Shapefile")
  
  if(returnShape==TRUE){
    return(route_shape)
  }
  
}
