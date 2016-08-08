#' Fetch transit dependent population statistics
#'
#' Requires dplyr,tidyr, and acs packages to be installed.
#'
#' @param geog Geography object (constructed using `geo.make()` from `acs` package)
#' @param endyear Endyear of ACS data request (default is 2014)
#' @param span Span of ACS estimate (default is five years)
#'
#' @return Dataframe with transit dependent population statistics.
#' @export
transit_pops = function(geog,endyear=2014, span=5){
  require(dplyr,quietly = TRUE)
  require(acs, quietly = TRUE)
  require(tidyr,quietly = TRUE)

  #Total Population
  tid = "B01003"
  result = acs.fetch(endyear = endyear,span=span,geography=geog,table.number =tid,col.names = "pretty")
  data = as_data_frame(result@estimate)

  base_geog = result@geography
  names = base_geog$NAME

  results = tbl_df(base_geog)

  results$total_pop = data$`Total Population: Total`
  print(paste0(round(1/8*100,0),"% done with queries"))

  #Low Income
  tid = "C17002"
  result = acs.fetch(endyear = endyear,span=span,geography=geog,table.number =tid,col.names = "pretty")
  data = as_data_frame(result@estimate)
  sub = data[,1:5]
  props = sub %>% dplyr::rename(total=`Ratio of Income to Poverty Level in the Past 12 Months: Total:`) %>%
    dplyr::mutate(below_150= `Ratio of Income to Poverty Level in the Past 12 Months: Under .50`+
             `Ratio of Income to Poverty Level in the Past 12 Months: .50 to .99`+
             `Ratio of Income to Poverty Level in the Past 12 Months: 1.00 to 1.24`+
             `Ratio of Income to Poverty Level in the Past 12 Months: 1.25 to 1.49`) %>%
    dplyr::select(total,below_150) %>% dplyr::mutate(below_150_prop= below_150/total)
  
  results$low_income_prop = props$below_150_prop 
  results$low_income_n = props$below_150
  print(paste0(round(2/8*100,0),"% done with queries"))

  #Older Adults
  tid = "B01001"
  result = acs.fetch(endyear = endyear,span=span,geography=geog,table.number =tid,col.names = "pretty")
  data = as_data_frame(result@estimate)
  sub = data %>% dplyr::select(contains("Total"),contains("65"), contains("67"),
                               contains("70"),contains("75"),contains("80"),
                               contains("85"))
  sub$geography = names
  props= sub %>% dplyr::rename(total=`Sex by Age: Total:`) %>%
    tidyr::gather(key,value,-geography,-total) %>%
    dplyr::distinct() %>%
    dplyr::group_by(geography,total) %>% dplyr::summarise(old = sum(value)) %>% 
    dplyr::ungroup() %>%
    dplyr::mutate(old_prop = old/total)

  results = results %>% left_join(props %>% dplyr::select(geography,old_prop,old) %>% 
                                    dplyr::rename(older_adults_prop=old_prop,older_adults_n=old,NAME=geography),by="NAME")
  print(paste0(round(3/8*100,0),"% done with queries"))

  #Youth 10-17
  tid = "B01001"
  result = acs.fetch(endyear = endyear,span=span,geography=geog,table.number =tid,col.names = "pretty")
  data = as_data_frame(result@estimate)
  sub = data %>% dplyr::select(contains("Total"), contains("10"), contains("17"))
  sub$geography = names
  props= sub %>% dplyr::rename(total=`Sex by Age: Total:`) %>%
    tidyr::gather(key,value,-geography,-total) %>% 
    dplyr::distinct() %>%
    dplyr::select(-key) %>%
    dplyr::group_by(geography,total) %>% dplyr::summarise(young = sum(value))  %>%
    dplyr::ungroup() %>% dplyr::mutate(young_prop = young/total)

  results = results %>% left_join(props %>% dplyr::select(geography,young_prop,young) %>% 
                                    dplyr::rename(youth_prop=young_prop,youth_n=young,NAME=geography),by="NAME")
  print(paste0(round(4/8*100,0),"% done with queries"))

  #College Age 18-24
  tid = "B01001"
  result = acs.fetch(endyear = endyear,span=span,geography=geog,table.number =tid,col.names = "pretty")
  data = as_data_frame(result@estimate)
  sub = data %>% dplyr::select(contains("Total"), contains("18"), contains("20"),
                        contains("21"),contains("22"))
  sub$geography = names
  props= sub %>% dplyr::rename(total=`Sex by Age: Total:`) %>%
    tidyr::gather(key,value,-geography,-total) %>% 
    dplyr::distinct() %>%
    dplyr::select(-key) %>%
    group_by(geography,total) %>% dplyr::summarise(young = sum(value)) %>% 
    dplyr::ungroup() %>%dplyr::mutate(young_prop = young/total)

  results = results %>% left_join(props %>% dplyr::select(geography,young_prop,young) %>% 
                                    dplyr::rename(college_age_prop=young_prop,college_age_n=young,NAME=geography),by="NAME")
  print(paste0(round(5/8*100,0),"% done with queries"))

  #Persons with disabilities
  tid = "C21007"
  result = acs.fetch(endyear = endyear,span=span,geography=geog,table.number =tid,col.names = "pretty")
  data = as_data_frame(result@estimate)
  sub = data %>% dplyr::select(contains("Total"), contains("With a disability"))
  sub$geography = names
  props= sub %>% dplyr::rename(total=`Age by Veteran Status by Poverty Status in the Past 12 Months by Disability Status for the Civilian Population 18+ Years: Total:`) %>%
    tidyr::gather(key,value,-geography,-total) %>% 
    dplyr::distinct() %>%
    dplyr::select(-key) %>%
    group_by(geography,total) %>% dplyr::summarise(disabled = sum(value)) %>% 
    dplyr::ungroup() %>% dplyr::mutate(disabled_prop = disabled/total)

  results = results %>% left_join(props %>% dplyr::select(geography,disabled_prop,disabled) %>% 
                                    dplyr::rename(pwd_18up_prop=disabled_prop,pwd_18up_n=disabled,NAME=geography),by="NAME")
  print(paste0(round(6/8*100,0),"% done with queries"))

  #Households without Vehicles
  tid = "B25044"
  result = acs.fetch(endyear = endyear,span=span,geography=geog,table.number =tid,col.names = "pretty")
  data = as_data_frame(result@estimate)
  sub = data %>% dplyr::select(contains("Total"), contains("No vehicle available"))
  sub$geography = names
  props= sub %>% dplyr::rename(total=`Tenure by Vehicles Available: Total:`) %>%
    tidyr::gather(key,value,-geography,-total) %>% 
    dplyr::distinct() %>%
    dplyr::select(-key) %>%
    group_by(geography,total) %>% dplyr::summarise(zero_vehicles = sum(value)) %>%
    dplyr::ungroup() %>% dplyr::mutate(zero_vehicles_prop = zero_vehicles/total)

  results = results %>% left_join(props %>% dplyr::select(geography,zero_vehicles_prop,zero_vehicles) %>% 
                                    dplyr::rename(no_vehicles_hh_prop=zero_vehicles_prop, no_vehicles_hh_n=zero_vehicles,NAME=geography),by="NAME")
  print(paste0(round(7/8*100,0),"% done with queries"))

  #Limited English
  tid = "B16004"
  result = acs.fetch(endyear = endyear,span=span,geography=geog,table.number =tid,col.names = "pretty")
  data = as_data_frame(result@estimate)
  sub = data %>% dplyr::select(contains("Total"), contains("not well"),contains("not at all"))
  sub$geography = names
  props= sub %>% dplyr::rename(total=`Age by Language Spoken at Home by Ability to Speak English for the Population 5+ Yrs: Total:`) %>%
    tidyr::gather(key,value,-geography,-total) %>% 
    dplyr::distinct() %>%
    dplyr::select(-key) %>%
    group_by(geography,total) %>% dplyr::summarise(no_english = sum(value)) %>% 
    dplyr::ungroup() %>% dplyr::mutate(no_english_prop = no_english/total)

  results = results %>% left_join(props %>% dplyr::select(geography,no_english_prop,no_english) %>% 
                                    dplyr::rename(lep_prop=no_english_prop,lep_n=no_english,NAME=geography),by="NAME")
  print(paste0(round(8/8*100,0),"% done with queries"))

  return(results)
}

#' Join Census Geography to ACS Results
#'
#' @param table Data frame of ACS results, such as the output of `transit_pops()`
#' @param geo_level A geographic level: county, place, tract, or block group
#'
#' @return SpatialPolygonsDataFrame with ACS data joined
#' @export
geo_join_acs = function(table,geo_level){
  if(geo_level=="place"){
    states = unique(table$state)
    if(length(states)>1){
      geom = tigris::places(state = states[1])
      geom = sp::spChFIDs(geom,as.character(1:length(geom)))
      nex = length(geom)+1
      for(i in 2:length(states)){
        next_geom = tigris::places(state = states[i])
        next_geom = sp::spChFIDs(next_geom,as.character(nex:(nex+length(next_geom)-1)))
        nex = length(next_geom)+1
        geom = maptools::spRbind(geom,next_geom)
      }
    }else{
      geom = tigris::places(state = states[1])
    }
    table = table %>% 
      mutate(GEOID=paste0(state,str_pad(place,side="left",width=5,pad="0")))
    full = geom
    full@data = data.frame(
      full@data %>% left_join(table,by="GEOID")
    )
    return(full)
  }else if(geo_level=="tract"){
    states = unique(table$state)
    if(length(states)>1){
      counties = unique(table$county[table$state==states[1]])
      geom = tigris::tracts(state = states[1],county = counties)
      geom = sp::spChFIDs(geom,as.character(1:length(geom)))
      nex = length(geom)+1
      for(i in 2:length(states)){
        counties = unique(table$county[table$state==states[i]])
        next_geom = tigris::tracts(state = states[i],county=counties)
        next_geom = sp::spChFIDs(next_geom,as.character(nex:(nex+length(next_geom)-1)))
        nex = length(next_geom)+1
        geom = maptools::spRbind(geom,next_geom)
      }
    }else{
      counties = unique(table$county[table$state==states[1]])
      geom = tigris::tracts(state = states[1],county = counties)
    }
    table = table %>% 
      mutate(GEOID=paste0(state,str_pad(county,side="left",width=3,pad="0"),tract))
    full = geom
    full@data = data.frame(
      full@data %>% left_join(table,by="GEOID")
    )
    return(full)
  }else if(geo_level=="block group"){
    states = unique(table$state)
    if(length(states)>1){
      counties = unique(table$county[table$state==states[1]])
      geom = tigris::block_groups(state = states[1],county = counties)
      geom = sp::spChFIDs(geom,as.character(1:length(geom)))
      nex = length(geom)+1
      for(i in 2:length(states)){
        counties = unique(table$county[table$state==states[i]])
        next_geom = tigris::block_groups(state = states[i],county=counties)
        next_geom = sp::spChFIDs(next_geom,as.character(nex:(nex+length(next_geom)-1)))
        nex = length(next_geom)+1
        geom = maptools::spRbind(geom,next_geom)
      }
    }else{
      counties = unique(table$county[table$state==states[1]])
      geom = tigris::block_groups(state = states[1],county = counties)
    }
    table = table %>% 
      mutate(GEOID=paste0(state,str_pad(county,side="left",width=3,pad="0"),
                          str_pad(tract,side="left",width=6,pad="0"),blockgroup))
    full = geom
    full@data = data.frame(
      full@data %>% left_join(table,by="GEOID")
    )
    return(full)
  }else if(geo_level=="county"){
    states = unique(table$state)
    if(length(states)>1){
      geom = tigris::counties(state = states[1])
      counties = unique(table$county[table$state==states[1]])
      geom = geom[geom$COUNTYFP %in% str_pad(counties,width=3,side="left",pad="0"),]
      geom = sp::spChFIDs(geom,as.character(1:length(geom)))
      nex = length(geom)+1
      for(i in 2:length(states)){
        next_geom = tigris::counties(state = states[i])
        counties = unique(table$county[table$state==states[i]])
        next_geom = next_geom[next_geom$COUNTYFP %in% str_pad(counties,width=3,side="left",pad="0"),]
        next_geom = sp::spChFIDs(next_geom,as.character(nex:(nex+length(next_geom)-1)))
        nex = length(next_geom)+1
        geom = maptools::spRbind(geom,next_geom)
      }
    }else{
      geom = tigris::counties(state = states[1])
      counties = unique(table$county[table$state==states[1]])
      geom = geom[geom$COUNTYFP %in% str_pad(counties,width=3,side="left",pad="0"),]
    }
    table = table %>% 
      mutate(GEOID=paste0(state,str_pad(county,side="left",width=3,pad="0")))
    full = geom
    full@data = data.frame(
      full@data %>% left_join(table,by="GEOID")
    )
    return(full)
  }else{
    stop("'geo_level' must be place, tract, block group, or county")
  }
}
