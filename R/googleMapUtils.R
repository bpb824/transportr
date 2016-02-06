#' Fetch Google directions from Directions API
#'
#' @param from Start location to fetch directions for (vector in [Lat Lng] format)
#' @param to End location to fetch directions for (vector in [Lat Lng] format)
#' @param key API key for Google Directions API (from Google Developers Console)
#' @param mode Travel mode: one of four strings: 'driving', 'walking', 'biking', 'transit'
#' @param departure Optional departure time of POSIXct class. Cannot specify both arrival and departure times.
#' @param arrival Optional arrival time of POSIXct class. Cannot specify both arrival and departure times.
#'
#' @return Encoded google directions string
#' @export
getGoogleDirections <- function(from, to, key, mode, departure=NULL, arrival=NULL,alternatives=FALSE) {

  if(!is.null(departure) & !is.null(arrival)){
    stop("Cannot specify both departure and arrival times.")
  }

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

  return(rjson::fromJSON(paste(readLines(url), collapse="")))
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
#' @export
travel_time= function(from,to,key,mode,departure = NULL, arrival = NULL){

  if(!is.null(departure) & !is.null(arrival)){
    stop("Cannot specify both departure and arrival times.")
  }

  if(!is.null(departure)){
    dirs =  getGoogleDirections(from,to,gKey,mode,departure = departure)
  }else if(!is.null(arrival)){
    dirs =  getGoogleDirections(from,to,gKey,mode,arrival = arrival)
  }

  if(dirs$status=="OK"){
    numRoutes = length(dirs$routes)
    print(paste0(numRoutes," routes returned successfully."))
    if(numRoutes>1){
      timeVector = vector()
      for(i in 1:numRoutes){
        timeVector[i] = dirs$routes[[i]]$legs[[1]]$duration$value/60
      }
      time = mean(timeVector)
    }else{
      time = dirs$routes[[i]]$legs[[1]]$duration$value/60
    }
  }else{
    stop("API returned no results. Check you inputs")
  }
}



#' Convert SpatialLines to coordinate data frame for map matching.
#'
#' @param linesShape SpatialLines shape containing trip to convert to points
#' @param trip_id ID of trip to convert to points
#' @param dummyDate Date/time string to use for dummy timestamp in YYYY-MM-DD HH:MM:SS format.
#'
#' @return Coordinate data frame to save as .csv for map matching.
#'
lines2points = function(linesShape,trip_id, dummyDate){

  numCoords = 0
  for (i in 1:length(linesShape)){
    numCoords = numCoords + nrow(linesShape@lines[[i]]@Lines[[1]]@coords)
  }

  crds = data.frame(matrix(nrow = numCoords,ncol = 9))
  colnames(crds)=c("id","trip_id","time","lat","long","alt","speed","hAcc","vAcc")
  crds$id = rownames(crds)
  crds$trip_id=trip_id
  startTime = as.POSIXct(strptime("2015-03-14  09:26:53",tz="America/Los_Angeles",format = "%Y-%m-%d %H:%M:%S"))
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
