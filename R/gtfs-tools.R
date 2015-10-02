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

