#' Fetch Google directions from Directions API
#'
#' @param from Start location to fetch directions for (vector in [Lat Lng] format)
#' @param to End location to fetch directions for (vector in [Lat Lng] format)
#' @param key API key for Google Directions API (from Google Developers Console)
#' @param mode Travel mode: one of four strings: 'driving', 'walking', 'bicycling', 'transit'
#' @param departure Optional departure time of POSIXct class. Cannot specify both arrival and departure times.
#' @param arrival Optional arrival time of POSIXct class. Cannot specify both arrival and departure times.
#' @param waypoints Optional additional locations to travel through along route. A data frame or with 'lat' and 'lng' columns, and an observation for each destination.
#'
#' @return Encoded google directions string
#' @export
getGoogleDirections <- function(from, to, key, mode, departure=NULL, arrival=NULL,
                                alternatives=FALSE, waypoints = NULL,transit_rt_pref=NULL,
                                traffic_model=NULL) {

  if(!is.null(departure) & !is.null(arrival)){
    stop("Cannot specify both departure and arrival times.")
  }

  if(is.null(waypoints)){
    to = paste0(to,collapse = ",")
    from = paste0(from,collapse=",")

    if(!is.null(departure)){
      time = as.numeric(departure)
      baseurl <- "https://maps.googleapis.com/maps/api/directions/json?origin="
      url = paste0(baseurl,from,"&destination=",to,"&mode=",mode,"&key=",key,"&departure_time=",time)
    }else if(!is.null(arrival)){
      time = as.numeric(arrival)
      baseurl <- "https://maps.googleapis.com/maps/api/directions/json?origin="
      url = paste0(baseurl,from,"&destination=",to,"&mode=",mode,"&key=",key,"&arrival_time=",time)
    }else{
      baseurl <- "https://maps.googleapis.com/maps/api/directions/json?origin="
      url = paste0(baseurl,from,"&destination=",to,"&mode=",mode,"&key=",key)
    }

    if(alternatives){
      url = paste0(url,"&alternatives=true")
    }
    
    if(!is.null(transit_rt_pref)){
      if(!(transit_rt_pref %in% c("less_walking","fewer_transfers"))){
        stop("check transit_rt_pref input")
      }
      url = paste0(url,"&transit_routing_preference=",transit_rt_pref)
    }
    
    if(!is.null(traffic_model)){
      url = paste0(url,"&traffic_model=",traffic_model)
    }
    

    return(rjson::fromJSON(paste(readLines(url), collapse="")))
  }else{

    to = paste0(to,collapse = ",")
    from = paste0(from,collapse=",")

    way_string = paste0("&via=",waypoints$lat[1],",",waypoints$lng[1])
    for(i in 1:length(waypoints)){
      way_string = paste0(way_string,"|",waypoints$lat[i],",",waypoints$lng[i])
    }

    if(!is.null(departure)){
      time = as.numeric(departure)
      baseurl <- "https://maps.googleapis.com/maps/api/directions/json?origin="
      url = paste0(baseurl,from,"&destination=",to,way_string,"&mode=",mode,"&key=",key,"&departure_time=",time)
    }else if(!is.null(arrival)){
      time = as.numeric(arrival)
      baseurl <- "https://maps.googleapis.com/maps/api/directions/json?origin="
      url = paste0(baseurl,from,"&destination=",to,way_string,"&mode=",mode,"&key=",key,"&arrival_time=",time)
    }else{
      baseurl <- "https://maps.googleapis.com/maps/api/directions/json?origin="
      url = paste0(baseurl,from,"&destination=",to,way_string,"&mode=",mode,"&key=",key)
    }

    if(alternatives){
      url = paste0(url,"&alternatives=true")
    }

    return(rjson::fromJSON(paste(readLines(url), collapse="")))

  }


}

#' Convert Google directions to Spatial Object
#'
#' @param googleDirs List of resuls from `getGoogleDirections()` function
#' @param mode Transportation mode of directions ('driving','walking','bicycling','transit')
#'
#' @return SpatialLinesDataFrame object representing Google directed routes
#'
#' @export
gDirsToShape= function(googleDirs,mode){

  src <- '
  std::string encoded = as<std::string>(a);

  int index = 0;
  int len = encoded.size();
  int df_index = 0;
  long double lat = 0;
  long double lng = 0;
  std::vector<long double> longitude(0);
  std::vector<long double> latitude(0);

  if(encoded.size() == 0)
  return R_NilValue;

  longitude.reserve(30000);
  latitude.reserve(30000);

  while(index < len) {
  int b;
  int shift = 0;
  int result = 0;

  do {
  b = encoded[index++] - 63;
  result |= (b & 0x1f) << shift;
  shift += 5;
  } while(b >= 0x20);
  long double dlat = ((result & 1) ? ~(result >> 1) : (result >> 1));
  lat += dlat;
  latitude.push_back(lat * 1e-5);

  shift = 0;
  result = 0;
  do {
  b = encoded[index++] - 63;
  result |= (b & 0x1f) << shift;
  shift += 5;
  } while(b >= 0x20);
  long double dlng = ((result & 1) ? ~(result >> 1) : (result >> 1));
  lng += dlng;
  longitude.push_back(lng * 1e-5);
  df_index++;
  }

  return DataFrame::create( _["lat"] = latitude,  _["lng"] = longitude );
  '


  if (require("Rcpp") & require("inline")){
    DecodeLine <- cxxfunction(signature(a = "character"),
                              src, plugin = "Rcpp")
  } else {
    DecodeLine <- decodeLineR
  }

  routes = googleDirs$routes
  routeList = list()
  idCount = 0

  if(mode=="transit"){
    for(i in 1:length(routes)){
      route = routes[[i]]
      leg = route$legs[[1]]
      steps = leg$steps
      lineList = list()
      route_data = data.frame(matrix(nrow=length(steps),ncol =6))
      colnames(route_data)=c("step_id","distance_mi","duration_min","description","transit_details",
                             "route_id")
      route_data$route_id=i
      rownames(route_data)=as.character(idCount:(idCount+nrow(route_data)-1))
      for(j in 1:length(steps)){
        step = steps[[j]]
        line_string = step$polyline$points
        poly = DecodeLine(line_string)
        polyLine = sp::Lines(sp::Line(cbind(poly$lng,poly$lat)),idCount)
        lineList[[j]]=polyLine
        route_data$step_id[j]=j
        route_data$distance_mi[j]=step$distance$value*0.000621371
        route_data$duration_min[j]=step$duration$value/60
        route_data$description[j]=step$html_instructions
        route_data$link_code[j] = line_string
        if("transit_details" %in% names(step)){
          route_data$transit_details[j]=paste0(step$transit_details$line$short_name," (",
                                               step$transit_details$line$name,")")
        }else{
          route_data$transit_details[j]=NA
        }

        idCount= idCount+1
      }
      polyShape = sp::SpatialLines(lineList,sp::CRS("+init=epsg:4326"))
      polyShapeDF = sp::SpatialLinesDataFrame(polyShape,route_data,match.ID = FALSE)
      routeList[[i]]=polyShapeDF
    }
  }else{
    for(i in 1:length(routes)){
      route = routes[[i]]
      leg = route$legs[[1]]
      steps = leg$steps
      lineList = list()
      route_data = data.frame(matrix(nrow=length(steps),ncol =5))
      colnames(route_data)=c("step_id","distance_mi","duration_min","description","route_id")
      route_data$route_id=i
      rownames(route_data)=as.character(idCount:(idCount+nrow(route_data)-1))
      for(j in 1:length(steps)){
        step = steps[[j]]
        line_string = step$polyline$points
        poly = DecodeLine(line_string)
        polyLine = sp::Lines(sp::Line(cbind(poly$lng,poly$lat)),idCount)
        lineList[[j]]=polyLine
        route_data$step_id[j]=j
        route_data$distance_mi[j]=step$distance$value*0.000621371
        route_data$duration_min[j]=step$duration$value/60
        route_data$description[j]=step$html_instructions
        route_data$link_code[j] = line_string
        idCount= idCount+1
      }
      polyShape = sp::SpatialLines(lineList,sp::CRS("+init=epsg:4326"))
      polyShapeDF = sp::SpatialLinesDataFrame(polyShape,route_data,match.ID = TRUE)
      routeList[[i]]=polyShapeDF
    }
  }

  routeShape = routeList[[1]]
  if(length(routeList)>1){
    for(i in 2:length(routeList)){
      routeShape = maptools::spRbind(routeShape,routeList[[i]])
    }
  }

  return(routeShape)

}

#' Title
#'
#' @param origins Data frame with origin data with column names 'id','lat','lng' or 'id','place' (if places=TRUE)
#' @param destinations Data frame with destination data with column names 'id','lat','lng' or 'id','place' (if places=TRUE)
#' @param places Boolean indicating if you are providing lat/lng locations or place names
#' @param key Google Maps API Key
#' @param mode Travel mode
#' @param departure POSIXct departure time
#' @param arrival POSIXct arrival time
#'
#' @return Data frame with travel times and distances identified by origin and destination id
#' @export
tt_matrix = function(origins,destinations,places = FALSE,key,mode,departure = NULL, arrival = NULL){

  #Constants
  base_url = "https://maps.googleapis.com/maps/api/distancematrix/json?origins="

  #Catch exceptions
  if(!is.null(departure) & !is.null(arrival)){
    stop("Cannot specify both departure and arrival times.")
  }
  if(!is.null(arrival) & mode =="driving"){
    stop("Cannot specify arrival time for driving travel time estimates")
  }

  #Geocode if handed places for origins and destinations
  if(places==TRUE){
    stop("Places functionality not ready yet")
  }

  #Travel time query and storage
  time_frame = data.frame(matrix(nrow=nrow(origins)*nrow(destinations),ncol = 5))
  colnames(time_frame)=c("origin_id","destination_id","tt_min","tt_traffic_min","distance_km")
  time_frame$origin_id=origins$id
  time_frame$destination_id=sort(rep(destinations$id,nrow(origins)))

  progress=0
  pb = txtProgressBar(min = 0, max = nrow(time_frame),style = 3)

  for(i in 1:nrow(origins)){
    oid = origins$id[i]
    for(j in 1:nrow(destinations)){
      did = destinations$id[j]
      origin = origins[i,c("lat","lng")]
      destination = destinations[j,c("lat","lng")]

      #Add origin and destination to url
      url = paste0(base_url,paste(origin,collapse = ","),"&destinations=",paste(destination,collapse = ","))

      #Add key to url
      url = paste0(url, "&key=",key)

      #Add mode to url
      url = paste0(url, "&mode=",mode)

      #Add departure/arrival time to url
      if(!is.null(departure)){
        url = paste0(url,"&departure_time=",departure)
      }else if(!is.null(arrival)){
        url = paste0(url,"&arrival_time=",arrival)
      }else{
        url = paste0(url,"&departure_time=",as.numeric(Sys.time()+60))
      }

      #Add traffic model to url
      url = paste0(url,"&traffic_model=best_guess")

      #Send query, parse response
      response = httr::content(httr::GET(url),as = "parsed", type ="application/json")
      if(response$rows[[1]]$elements[[1]]$status!="ZERO_RESULTS"){
        time_frame$distance_km[time_frame$origin_id==oid & time_frame$destination_id==did]=response$rows[[1]]$elements[[1]]$distance$value/1000
        time_frame$tt_min[time_frame$origin_id==oid & time_frame$destination_id==did]=response$rows[[1]]$elements[[1]]$duration$value/60
        if(mode=="driving"){
          time_frame$tt_traffic_min[time_frame$origin_id==oid & time_frame$destination_id==did]=response$rows[[1]]$elements[[1]]$duration_in_traffic$value/60
        }
      }
      progress = progress +1
      setTxtProgressBar(pb,progress)
    }
  }

  close(pb)

  if(mode == "transit"){
    time_frame = time_frame %>% select(-tt_traffic_min)
  }

  return(time_frame)
}


#' Query travel times from Google API
#'
#' @param from Start location to fetch directions for (vector in [Lat Lng] format)
#' @param to End location to fetch directions for (vector in [Lat Lng] format)
#' @param key API key for Google Directions API (from Google Developers Console)
#' @param mode Travel mode: one of four strings: 'driving', 'walking', 'biking', 'transit'
#' @param departure Optional departure time of POSIXct class. Cannot specify both arrival and departure times.
#' @param arrival Optional arrival time of POSIXct class. Cannot specify both arrival and departure times.
#'
#' @return Travel time (minutes)
#'
#' @export
travel_time = function(from,to,key,mode,departure = NULL, arrival = NULL,tt_type="fastest"){

  if(!is.null(departure) & !is.null(arrival)){
    stop("Cannot specify both departure and arrival times.")
  }

  if(tt_type=="fastest"){
    if(!is.null(departure)){
      dirs =  getGoogleDirections(from,to,gKey,mode,departure = departure)
    }else if(!is.null(arrival)){
      dirs =  getGoogleDirections(from,to,gKey,mode,arrival = arrival)
    }else{
      dirs =  getGoogleDirections(from,to,gKey,mode)
    }

    if(dirs$status=="OK"){
      numRoutes = length(dirs$routes)
      print(paste0(numRoutes," routes returned successfully."))
      if(numRoutes>1){
        timeVector = vector()
        for(i in 1:numRoutes){
          timeVector[i] = dirs$routes[[i]]$legs[[1]]$duration$value/60
        }
        return(mean(timeVector))
      }else{
        return(dirs$routes[[1]]$legs[[1]]$duration$value/60)
      }
    }else{
      stop("API returned no results. Check you inputs")
    }
  }else if(tt_type=="average"){
    if(!is.null(departure)){
      dirs =  getGoogleDirections(from,to,gKey,mode,departure = departure,alternatives = TRUE)
    }else if(!is.null(arrival)){
      dirs =  getGoogleDirections(from,to,gKey,mode,arrival = arrival,alternatives = TRUE)
    }

    if(dirs$status=="OK"){
      numRoutes = length(dirs$routes)
      print(paste0(numRoutes," routes returned successfully."))
      if(numRoutes>1){
        timeVector = vector()
        for(i in 1:numRoutes){
          timeVector[i] = dirs$routes[[i]]$legs[[1]]$duration$value/60
        }
        return(mean(timeVector))
      }else{
        return(dirs$routes[[1]]$legs[[1]]$duration$value/60)
      }
    }else{
      stop("API returned no results. Check your inputs")
    }
  }
}

#' Geocode a place name using Google Places API
#'
#' @param placeString A string describing the place you'd like to geocode
#' @param output  The output can either be a simple location ('loc') or a list with all response results ('all')
#'
#' @return Location or list of API results
#' @export
geocode_place= function(placeString,key,output="loc"){

  place = gsub(" ","+",placeString)
  base_url = "https://maps.googleapis.com/maps/api/place/textsearch/json?query="
  query = paste0(base_url,place,"&key=",key)
  response = httr::content(httr::GET(query),as = "parsed", type ="application/json")
  if(output=="loc"){
    return(unlist(response$results[[1]]$geometry$location))
  }else if(output=="all"){
    return(response)
  }
}



#' Convert SpatialLines to coordinate data frame for map matching.
#'
#' @param linesShape SpatialLines shape containing trip to convert to points
#' @param trip_id ID of trip to convert to points
#'
#' @return Coordinate data frame to save as .csv for map matching.
#'
lines2points = function(linesShape,trip_id){

  numCoords = 0
  for (i in 1:length(linesShape)){
    numCoords = numCoords + nrow(linesShape@lines[[i]]@Lines[[1]]@coords)
  }

  crds = data.frame(matrix(nrow = numCoords,ncol = 9))
  colnames(crds)=c("id","trip_id","time","lat","long","alt","speed","hAcc","vAcc")
  crds$id = rownames(crds)
  crds$trip_id=trip_id
  startTime = as.POSIXct(Sys.time())
  endTime = startTime +(numCoords-1)*2
  timeVector = timeDate::timeSequence(startTime,endTime,by="2 sec",FinCenter = "America/Los_Angeles")
  crds$time=as.character(timeVector)
  crds$alt=100
  crds$speed=5
  crds$hAcc=5
  crds$vAcc=5

  cn = 1
  for (i in 1:length(linesShape)){
    segCoords = linesShape@lines[[i]]@Lines[[1]]@coords
    crds[cn:(cn+nrow(segCoords)-1),c("lat","long")]=cbind(segCoords[,2],segCoords[,1])
    cn=cn+nrow((segCoords))
  }
  return(crds)
}

#Google polyline decoder borrowed from:
#http://facstaff.unca.edu/mcmcclur/GoogleMaps/EncodePolyline/decode.js
#' Google Polyline Decoder
#'
#' @param encoded Encoded polyline result from Google Directions API request
#'
#' @return Decoded polyline.
#' @export
#'
#' @references Function adapted from http://facstaff.unca.edu/mcmcclur/GoogleMaps/EncodePolyline/decode.js and https://gist.github.com/diegovalle/916889/895dba68c0f9f5398c9d9c75856126e233b9acd7
decodeLineR <- function(encoded) {

  len = stringr::str_length(encoded)
  encoded <- strsplit(encoded, NULL)[[1]]
  index = 1
  N <- 100000
  df.index <- 1
  array = matrix(nrow = N, ncol = 2)
  lat = 0
  dlat = 0
  lng = 0
  dlng = 0
  b = 0
  shift = 0
  result = 0

  while(index <= len) {
    shift = 0
    result = 0

    repeat {
      b = as.integer(charToRaw(encoded[index])) - 63
      index <- index + 1
      result = bitops::bitOr(result, bitops::bitShiftL(bitops::bitAnd(b, 0x1f), shift))
      shift = shift + 5
      if(b < 0x20) break
    }
    dlat = ifelse(bitops::bitAnd(result, 1),
                  -(result - (bitops::bitShiftR(result, 1))),
                  bitops::bitShiftR(result, 1))
    lat = lat + dlat;

    shift = 0
    result = 0
    b = 0
    repeat {
      b = as.integer(charToRaw(encoded[index])) - 63
      index <- index + 1
      result = bitops::bitOr(result, bitops::bitShiftL(bitAnd(b, 0x1f), shift))
      shift = shift + 5
      if(b < 0x20) break
    }
    dlng = ifelse(bitops::bitAnd(result, 1),
                  -(result - (bitops::bitShiftR(result, 1))),
                  bitops::bitShiftR(result, 1))
    lng = lng + dlng

    array[df.index,] <- c(lat = lat * 1e-05, lng = lng * 1e-5)
    df.index <- df.index + 1
  }

  ret <- data.frame(array[1:df.index - 1,])
  names(ret) <- c("lat", "lng")
  return(ret)
}


#' Convert decoded Google Directions polyline into SpatialLines object
#'
#' @param directions Decoded polyline directions from Google Directions API
#'
#' @return SpatialLines object representing route received from Google Directions API
#' @export

route2shape = function(directions){

  src <- '
  std::string encoded = as<std::string>(a);

  int index = 0;
  int len = encoded.size();
  int df_index = 0;
  long double lat = 0;
  long double lng = 0;
  std::vector<long double> longitude(0);
  std::vector<long double> latitude(0);

  if(encoded.size() == 0)
  return R_NilValue;

  longitude.reserve(30000);
  latitude.reserve(30000);

  while(index < len) {
  int b;
  int shift = 0;
  int result = 0;

  do {
  b = encoded[index++] - 63;
  result |= (b & 0x1f) << shift;
  shift += 5;
  } while(b >= 0x20);
  long double dlat = ((result & 1) ? ~(result >> 1) : (result >> 1));
  lat += dlat;
  latitude.push_back(lat * 1e-5);

  shift = 0;
  result = 0;
  do {
  b = encoded[index++] - 63;
  result |= (b & 0x1f) << shift;
  shift += 5;
  } while(b >= 0x20);
  long double dlng = ((result & 1) ? ~(result >> 1) : (result >> 1));
  lng += dlng;
  longitude.push_back(lng * 1e-5);
  df_index++;
  }

  return DataFrame::create( _["lat"] = latitude,  _["lng"] = longitude );
  '


  if (require("Rcpp") & require("inline")){
    DecodeLine <- cxxfunction(signature(a = "character"),
                              src, plugin = "Rcpp")
  } else {
    DecodeLine <- decodeLineR
  }


  route = directions$routes[[1]]
  steps = route$legs[[1]]$steps
  poly = DecodeLine(steps[[1]]$polyline$points)
  polyLine = sp::Lines(sp::Line(cbind(poly$lng,poly$lat)),1)
  lineList = list()
  lineList[[1]]=polyLine
  if (length(steps)>1){
    for (i in 2:length(steps)){
      poly = DecodeLine(steps[[i]]$polyline$points)
      polyLine = sp::Lines(sp::Line(cbind(poly$lng,poly$lat)),i)
      lineList[[i]]=polyLine
    }
  }
  polyShape = sp::SpatialLines(lineList,sp::CRS("+init=epsg:4326"))
  return(polyShape)
}
