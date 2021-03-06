#' Fetch GTFS feed files
#'
#' This function will match the requested transit agency name with a feed via the GTFS data exchange, and the latest feed files will be downloaded to 'feeds' subfolder of working directory.
#'
#' @param feedName Name of transit agency (will try to find similar names if not exact match)
#' @param outDir The directory in which to save the downloaded feed
#'
#' @return None
#' @export
fetchFeed = function(feedName,outDir="."){
  #url = "http://www.gtfs-data-exchange.com/api/agencies"
  url = "https://transit.land/api/v1/operators?per_page=1000"
  gtfsFeeds = httr::content(httr::GET(url),as = "parsed", type ="application/json")

  names =unlist(lapply(gtfsFeeds$operators,function(x) x$name))
  test = grep(tolower(feedName), tolower(names))

  if(length(test)==0){
    stop("Could not find the feed you are looking for. The available feeds are listed here: https://transit.land/feed-registry/")
  }else if(length(test)==1){
    operator = names[test]
    meta = gtfsFeeds$operators[[test]]
    id = meta$represented_in_feed_onestop_ids[[1]]
    url = paste0("https://transit.land/api/v1/feeds/",id)
    feedInfo = httr::content(httr::GET(url),as = "parsed", type ="application/json")
    feed_url = feedInfo$url
    if(!dir.exists(paste0(outDir,"/feeds"))){
      dir.create(paste0(outDir,"/feeds"))
    }
    if(!dir.exists(paste0(outDir,"/feeds/",meta$name))){
      dir.create(paste0(outDir,"/feeds/",meta$name))
    }
    download.file(feed_url,paste0(outDir,"/feeds/",meta$name,"/feed.zip"))
    unzip(paste0(outDir,"/feeds/",meta$name,"/feed.zip"), exdir=paste0(outDir,"/feeds/",meta$name))
    print(paste0("Feed downloaded and unzipped to ",outDir,"/feeds/",meta$name))
  }else{
    print("More than one result available. Here are the names of the transit agencies found from your search term:")
    print(names[test])
  }
}

#' Export GTFS routes to ESRI shapefile
#'
#' @param feedPath The relative or absolute path to the folder containing the GTFS feed text files
#' @param outPath The relative or absolute path to the folder where you would like to save the reuslting shapefile
#' @param shapeName The filename for your resulting shapefile
#' @param writeShapefile Boolean indicating if route shape should be written to ESRI Shapefile
#'
#' @return Returns route shape as SpatialLinesDataFrame if desired
#' @export
exportRouteShape = function(feedPath,outPath= NULL,shapeName = NULL,writeShapefile=FALSE){
  
  #feedPath="R/Translink GTFS"
  
  shapes = read.csv(paste0(feedPath,"/shapes.txt"),stringsAsFactors = FALSE)
  routes = read.csv(paste0(feedPath,"/routes.txt"),stringsAsFactors = FALSE)
  trips = read.csv(paste0(feedPath,"/trips.txt"),stringsAsFactors = FALSE)
  
  shapeCoords = list()
  
  shapeList = sort(unique(trips$shape_id))
  for (i in 1:length(shapeList)){
    shapeCoords[[as.character(shapeList[i])]]= shapes[shapes$shape_id == shapeList[i],]
  }
  lineList = list()
  sc = 1
  for (i in 1:length(shapeCoords)){
    id = names(shapeCoords[i])
    if(nrow(shapeCoords[[i]])>0){
      
      shape_frame = shapeCoords[[i]] %>%
        arrange(shape_pt_sequence)
      
      linestring =  sf::st_linestring(as.matrix(shape_frame[,c("shape_pt_lon",c("shape_pt_lat"))]))
      #shape = sp::Line(as.data.frame(shapeCoords[[i]])[,c("shape_pt_lon",c("shape_pt_lat"))])
      #lineList[[sc]]= sp::Lines(list(sp::Line(shape)),id)
      lineList[[sc]]= linestring
      sc = sc+1
    }
  }
  
  transitShapes =  sf::st_sfc(lineList,crs=4326)
  # transitShapes = sf::st_sf(feature=sf::st_sfc(lineList,crs = 4326),
  #                           shape_id=shape_ids,relation_to_geometry = c("field"),
  #                           stringsAsFactors = FALSE) %>%
  #   mutate(shape_id = as.numeric(shape_id))
  #transitShapes = sp::SpatialLines(lineList, sp::CRS("+init=epsg:4326"))

  shapeIds = sort(unique(trips$shape_id))
  shapeData = data.frame(matrix(nrow = length(shapeIds),ncol =8))
  colnames(shapeData)=c("shape_id","route_id","direction_id","route_short_name","route_long_name","route_type","route_url","route_color")
  shapeData$shape_id = shapeIds
  for (i in 1:nrow(shapeData)){
    sid = shapeData$shape_id[i]
    shapeData$route_id[i] = trips$route_id[trips$shape_id==sid][1]
    shapeData$direction_id[i] = trips$direction_id[trips$shape_id==sid][1]
    shapeData$route_short_name[i]=routes$route_short_name[routes$route_id==shapeData$route_id[i]]
    shapeData$route_long_name[i]=routes$route_long_name[routes$route_id==shapeData$route_id[i]]
    shapeData$route_type[i]=routes$route_type[routes$route_id==shapeData$route_id[i]]
    if("route_url" %in% colnames(routes)){
      shapeData$route_url[i]=routes$route_url[routes$route_id==shapeData$route_id[i]]
    }
    if("route_color" %in% colnames(routes)){
      shapeData$route_color[i]=routes$route_color[routes$route_id==shapeData$route_id[i]]
    }
    #print(i)
  }

  shapeData = shapeData %>%
    mutate(route_color=ifelse(nchar(route_color)==0,"#00659A",paste0("#",route_color)))
  
  shape_sf = sf::st_sf(shapeData,geometry=transitShapes)
  
  # leaflet() %>%
  #   addProviderTiles("CartoDB.Positron") %>%
  #   addPolylines(data=shape_sf)
   
  if(writeShapefile==TRUE){
    sf::st_write(shape_sf,outPath,shapeName,
                 driver = "ESRI Shapefile",layer_options = "OVERWRITE=true")
    #rgdal::writeOGR(route_shape,outPath,shapeName,overwrite_layer = TRUE,driver = "ESRI Shapefile")
  }

  return(shape_sf)
}

#' Export GTFS stops to ESRI shapefile
#'
#' @param feedPath The relative or absolute path to the folder containing the GTFS feed text files
#' @param outPath The relative or absolute path to the folder where you would like to save the reuslting shapefile
#' @param shapeName The filename for your resulting shapefile
#' @param returnShape  Boolean indicating if route shape should be written to ESRI Shapefile
#'
#' @return Returns stops shape as SpatialPointsDataFrame if desired
#' @export
exportStopShape = function(feedPath,outPath= NULL,shapeName = NULL,writeShapefile=FALSE){
  stops = read.csv(paste0(feedPath,"/stops.txt"),stringsAsFactors = FALSE)
  
  stop_shape = sf::st_as_sf(stops,coords = c("stop_lon","stop_lat"),crs=4326)
  
  if(writeShapefile==TRUE){
    sf::st_write(stop_shape,outPath,shapeName,
                 driver = "ESRI Shapefile",layer_options = "OVERWRITE=true")
  }
  return(stop_shape)
}

#' Import GTFS tables into a single Excel file
#'
#' @param feedPath A relative or absolute file path to the feed you want to import into Excel
#'
#' @return Writes Excel file to directory of feed
#' @export
gtfs_to_excel = function(feedPath){
  files = list.files(feedPath)
  wb = XLConnect::loadWorkbook(paste0(feedPath,"/","gtfs_all.xlsx"),create = TRUE)
  txts = files[grepl(".txt",files)]
  for(i in 1:length(txts)){
    file = txts[i]
    sheetName = gsub(".txt","",file)
    XLConnect::createSheet(wb,name=sheetName)
    data = read.csv(paste0(feedPath,"/",file))
    XLConnect::writeWorksheet(wb,data,sheet = sheetName)
    print(i)
  }
  XLConnect::saveWorkbook(wb)
}

