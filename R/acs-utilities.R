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

#' Fetch LODES data crosswalk for given state
#'
#' @param state_code Two character state code (string)
#'
#' @return Data frame containing LODES crosswalk
#' @export
lodes_crosswalk = function(state_code){
  state_code = tolower(state_code)
  crosswalk = readr::read_csv(paste0("http://lehd.ces.census.gov/data/lodes/LODES7/",
                                     state_code,"/",state_code,"_xwalk.csv.gz"))
}

#' Fetch LODES data files for given state and year (OD, RAC, or WAC)
#'
#' @param state_code Two character state code (string)
#' @param file_type Character string indicating what file type you want to fetch ('OD','WAC', or 'RAC')
#' @param year Year of interest
#'
#' @return Data frame containing all LODES data requested
#' @export
lodes_fetch = function(state_code,file_type,year){
  state_code = tolower(state_code)
  year = as.character(year)

  if(file_type=="OD"){
    response = httr::GET(paste0("http://lehd.ces.census.gov/data/lodes/LODES7/",state_code,"/od/"))
    page = httr::content(response,"text")
    parsed = xml2::read_html(page)
    body = xml_child(parsed,2)
    table = xml_contents(xml_child(body,"table"))
    files = vector()
    for(i in 1:length(table)){
      file = character()
      content = xml_contents(table[[i]])
      text =xml_text(content)
      file = text[grepl("csv",text)]
      if(length(file)>0){
        files = c(files,file)
      }
      #print(i)
    }

    year_files = files[grepl(year,files)]

    df_list = list()

    for(i in 1:length(year_files)){
      file = year_files[i]
      df = read_csv(paste0("http://lehd.ces.census.gov/data/lodes/LODES7/",state_code,"/od/",file))
      split = unlist(strsplit(file,"_",fixed=TRUE))
      part = split[3]
      type = split[4]
      df$part=part
      df$type = type
      df$w_geocode= as.character(df$w_geocode)
      df$h_geocode=as.character(df$h_geocode)
      df_list[[i]]=df
      #print(i)
    }

    od_data = bind_rows(df_list)
    return(od_data)

  }else if(file_type=="WAC"){
    response = httr::GET(paste0("http://lehd.ces.census.gov/data/lodes/LODES7/",state_code,"/wac/"))
    page = httr::content(response,"text")
    parsed = xml2::read_html(page)
    body = xml_child(parsed,2)
    table = xml_contents(xml_child(body,"table"))
    files = vector()
    for(i in 1:length(table)){
      file = character()
      content = xml_contents(table[[i]])
      text =xml_text(content)
      file = text[grepl("csv",text)]
      if(length(file)>0){
        files = c(files,file)
      }
      print(i)
    }

    year_files = files[grepl(year,files)]

    df_list = list()

    for(i in 1:length(year_files)){
      file = year_files[i]
      df = read_csv(paste0("http://lehd.ces.census.gov/data/lodes/LODES7/",state_code,"/wac/",file))
      split= unlist(strsplit(file,"_",fixed=TRUE))
      seg = split[3]
      type = split[4]
      df$seg=seg
      df$type = type
      df_list[[i]]=df
      print(i)
    }

    wac_data = bind_rows(df_list)
    return(wac_data)

  }else if(file_type=="RAC"){
    response = httr::GET(paste0("http://lehd.ces.census.gov/data/lodes/LODES7/",state_code,"/rac/"))
    page = httr::content(response,"text")
    parsed = xml2::read_html(page)
    body = xml_child(parsed,2)
    table = xml_contents(xml_child(body,"table"))
    files = vector()
    for(i in 1:length(table)){
      file = character()
      content = xml_contents(table[[i]])
      text =xml_text(content)
      file = text[grepl("csv",text)]
      if(length(file)>0){
        files = c(files,file)
      }
      print(i)
    }

    year_files = files[grepl(year,files)]

    df_list = list()

    for(i in 1:length(year_files)){
      file = year_files[i]
      df = read_csv(paste0("http://lehd.ces.census.gov/data/lodes/LODES7/",state_code,"/rac/",file))
      split= unlist(strsplit(file,"_",fixed=TRUE))
      seg = split[3]
      type = split[4]
      df$seg=seg
      df$type = type
      df_list[[i]]=df
      print(i)
    }

    rac_data = bind_rows(df_list)
    return(rac_data)

  }else{
    stop("'file_type' must be OD, WAC, or RAC")
  }
}

#' Aggregate LODES data to higher geography
#'
#' @param table LODES data frame (result of `lodes_fetch()`)
#' @param type Character string indicating what LODES file type you are aggregating ('OD','WAC', or 'RAC')
#' @param geo_level Character string indicating what geographic level to aggregate to (block group, tract, place, or county)
#' @param crosswalk Crosswalk data frame (result of `lodes_crosswalk()`)
#'
#' @return Data frame of aggregated LODES data
#'
#' @export
lodes_agg = function(table,type,geo_level,crosswalk) {

  if(geo_level=="block group"){
    if(type =="OD"){
      agg = table %>%
        left_join(crosswalk %>% select(tabblk2010,bgrp,bgrpname) %>%
                    rename(w_geocode = tabblk2010,w_bgrp = bgrp,
                           w_bgrpname=bgrpname),by="w_geocode") %>%
        left_join(crosswalk %>% select(tabblk2010,bgrp,bgrpname) %>%
                    rename(h_geocode = tabblk2010,h_bgrp = bgrp,
                           h_bgrpname=bgrpname),by="h_geocode") %>%
        select(-w_geocode,-h_geocode,-createdate) %>%
        group_by(h_bgrp,h_bgrpname,w_bgrp,w_bgrpname,type,part) %>% summarise_all(funs(sum))
    }else if(type == "WAC"){
      agg = table %>%
        left_join(crosswalk %>% select(tabblk2010,bgrp,bgrpname) %>%
                    rename(w_geocode = tabblk2010,w_bgrp = bgrp,
                           w_bgrpname=bgrpname),by="w_geocode") %>%
        select(-w_geocode,-createdate) %>%
        group_by(w_bgrp,w_bgrpname,type,seg) %>% summarise_all(funs(sum))
    }else if(type == "RAC"){
      agg = table %>%
        left_join(crosswalk %>% select(tabblk2010,bgrp,bgrpname) %>%
                    rename(h_geocode = tabblk2010,h_bgrp = bgrp,
                           h_bgrpname=bgrpname),by="h_geocode") %>%
        select(-h_geocode,-createdate) %>%
        group_by(h_bgrp,h_bgrpname,type,seg) %>% summarise_all(funs(sum))
    }
  }else if(geo_level == "tract"){
    if(type =="OD"){
      agg = table %>%
        left_join(crosswalk %>% select(tabblk2010,trct,trctname) %>%
                    rename(w_geocode = tabblk2010,w_trct = trct,
                           w_trctname=trctname),by="w_geocode") %>%
        left_join(crosswalk %>% select(tabblk2010,trct,trctname) %>%
                    rename(h_geocode = tabblk2010,h_trct = trct,
                           h_trctname=trctname),by="h_geocode") %>%
        select(-w_geocode,-h_geocode,-createdate) %>%
        group_by(h_trct,h_trctname,w_trct,w_trctname,type,part) %>% summarise_all(funs(sum))
    }else if(type == "WAC"){
      agg = table %>%
        left_join(crosswalk %>% select(tabblk2010,trct,trctname) %>%
                    rename(w_geocode = tabblk2010,w_trct = trct,
                           w_trctname=trctname),by="w_geocode") %>%
        select(-w_geocode,-createdate) %>%
        group_by(w_trct,w_trctname,type,seg) %>% summarise_all(funs(sum))
    }else if(type == "RAC"){
      agg = table %>%
        left_join(crosswalk %>% select(tabblk2010,trct,trctname) %>%
                    rename(h_geocode = tabblk2010,h_trct = trct,
                           h_trctname=trctname),by="h_geocode") %>%
        select(-h_geocode,-createdate) %>%
        group_by(h_trct,h_trctname,type,seg) %>% summarise_all(funs(sum))
    }

  }else if (geo_level == "place"){
    if(type =="OD"){
      print("Warning: blocks not belonging to Census Designated Places will be dropped")
      agg = table %>%
        left_join(crosswalk %>% select(tabblk2010,stplc,stplcname) %>%
                    rename(w_geocode = tabblk2010,w_stplc = stplc,
                           w_stplcname=stplcname),by="w_geocode") %>%
        left_join(crosswalk %>% select(tabblk2010,stplc,stplcname) %>%
                    rename(h_geocode = tabblk2010,h_stplc = stplc,
                           h_stplcname=stplcname),by="h_geocode") %>%
        filter(!is.na(h_stplcname),!is.na(w_stplcname))%>%
        select(-w_geocode,-h_geocode,-createdate) %>%
        group_by(h_stplc,h_stplcname,w_stplc,w_stplcname,type,part) %>% summarise_all(funs(sum))
    }else if(type == "WAC"){
      agg = table %>%
        left_join(crosswalk %>% select(tabblk2010,stplc,stplcname) %>%
                    rename(w_geocode = tabblk2010,w_stplc = stplc,
                           w_stplcname=stplcname),by="w_geocode") %>%
        mutate(w_stplcname = ifelse(is.na(w_stplcname),"Non-CDP",w_stplcname))%>%
        select(-w_geocode,-createdate) %>%
        group_by(w_stplc,w_stplcname,type,seg) %>% summarise_all(funs(sum))
    }else if(type == "RAC"){
      agg = table %>%
        left_join(crosswalk %>% select(tabblk2010,stplc,stplcname) %>%
                    rename(h_geocode = tabblk2010,h_stplc = stplc,
                           h_stplcname=stplcname),by="h_geocode") %>%
        mutate(h_stplcname = ifelse(is.na(h_stplcname),"Non-CDP",h_stplcname))%>%
        select(-h_geocode,-createdate) %>%
        group_by(h_stplc,h_stplcname,type,seg) %>% summarise_all(funs(sum))
    }
  }else if(geo_level == "county"){
    if(type =="OD"){
      agg = table %>%
        left_join(crosswalk %>% select(tabblk2010,cty,ctyname) %>%
                    rename(w_geocode = tabblk2010,w_cty = cty,
                           w_ctyname=ctyname),by="w_geocode") %>%
        left_join(crosswalk %>% select(tabblk2010,cty,ctyname) %>%
                    rename(h_geocode = tabblk2010,h_cty = cty,
                           h_ctyname=ctyname),by="h_geocode") %>%
        select(-w_geocode,-h_geocode,-createdate) %>%
        group_by(h_cty,h_ctyname,w_cty,w_ctyname,type,part) %>% summarise_all(funs(sum))
    }else if(type == "WAC"){
      agg = table %>%
        left_join(crosswalk %>% select(tabblk2010,cty,ctyname) %>%
                    rename(w_geocode = tabblk2010,w_cty = cty,
                           w_ctyname=ctyname),by="w_geocode") %>%
        select(-w_geocode,-createdate) %>%
        group_by(w_cty,w_ctyname,type,seg) %>% summarise_all(funs(sum))
    }else if(type == "RAC"){
      agg = table %>%
        left_join(crosswalk %>% select(tabblk2010,cty,ctyname) %>%
                    rename(h_geocode = tabblk2010,h_cty = cty,
                           h_ctyname=ctyname),by="h_geocode") %>%
        select(-h_geocode,-createdate) %>%
        group_by(h_cty,h_ctyname,type,seg) %>% summarise_all(funs(sum))
    }
  }
  return(agg)
}

#' Join aggregated lodes data to geography
#'
#' @param agg_table Aggregated LODES data frame (result of `lodes_agg()`)
#' @param geo_level Geographic level
#' @param crosswalk Crosswalk
#' @param acs_geom ACS geometry from ACS fetching/joining process
#'
#' @return SpatialPolygonsDataFrame with LODES data joined
#' @export
geo_join_lodes = function(agg_table,type,geo_level,crosswalk,acs_geom=NULL){
  if(type == "WAC"){
    prefix = "w_"
  }else if(type =="RAC"){
    prefix = "h_"
  }else{
    stop("Only RAC or WAC types are accepted.")
  }

  if(!is.null(acs_geom)){
    stop("Feature not finished yet")
    if(geo_level=="place"){
      states = unique(acs_geom$STATE)
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
      table = agg_table %>%
        mutate(GEOID=stplc)
      full = geom
      full@data = data.frame(
        full@data %>% left_join(table,by="GEOID")
      )
      return(full)
    }else if(geo_level=="tract"){
      states = unique(crosswalk$st)
      if(length(states)>1){
        counties = unique(agg_table$county[table$state==states[1]])
        geom = tigris::tracts(state = states[1],county = counties)
        geom = sp::spChFIDs(geom,as.character(1:length(geom)))
        nex = length(geom)+1
        for(i in 2:length(states)){
          counties = unique(agg_table$county[table$state==states[i]])
          next_geom = tigris::tracts(state = states[i],county=counties)
          next_geom = sp::spChFIDs(next_geom,as.character(nex:(nex+length(next_geom)-1)))
          nex = length(next_geom)+1
          geom = maptools::spRbind(geom,next_geom)
        }
      }else{
        counties = unique(agg_table$county[table$state==states[1]])
        geom = tigris::tracts(state = states[1],county = counties)
      }
      table = agg_table %>%
        mutate(GEOID=trct)
      full = geom
      full@data = data.frame(
        full@data %>% left_join(table,by="GEOID")
      )
      return(full)
    }else if(geo_level=="block group"){
      states = unique(crosswalk$st)
      if(length(states)>1){
        counties = unique(agg_table$county[table$state==states[1]])
        geom = tigris::block_groups(state = states[1],county = counties)
        geom = sp::spChFIDs(geom,as.character(1:length(geom)))
        nex = length(geom)+1
        for(i in 2:length(states)){
          counties = unique(agg_table$county[table$state==states[i]])
          next_geom = tigris::block_groups(state = states[i],county=counties)
          next_geom = sp::spChFIDs(next_geom,as.character(nex:(nex+length(next_geom)-1)))
          nex = length(next_geom)+1
          geom = maptools::spRbind(geom,next_geom)
        }
      }else{
        counties = unique(agg_table$county[table$state==states[1]])
        geom = tigris::block_groups(state = states[1],county = counties)
      }
      table = agg_table %>%
        mutate(GEOID=bgrp)
      full = geom
      full@data = data.frame(
        full@data %>% left_join(table,by="GEOID")
      )
      return(full)
    }else if(geo_level=="county"){
      states = unique(crosswalk$st)
      if(length(states)>1){
        geom = tigris::counties(state = states[1])
        counties = unique(agg_table$county[table$state==states[1]])
        geom = geom[geom$COUNTYFP %in% str_pad(counties,width=3,side="left",pad="0"),]
        geom = sp::spChFIDs(geom,as.character(1:length(geom)))
        nex = length(geom)+1
        for(i in 2:length(states)){
          next_geom = tigris::counties(state = states[i])
          counties = unique(agg_table$county[table$state==states[i]])
          next_geom = next_geom[next_geom$COUNTYFP %in% str_pad(counties,width=3,side="left",pad="0"),]
          next_geom = sp::spChFIDs(next_geom,as.character(nex:(nex+length(next_geom)-1)))
          nex = length(next_geom)+1
          geom = maptools::spRbind(geom,next_geom)
        }
      }else{
        geom = tigris::counties(state = states[1])
        counties = unique(agg_table$county[table$state==states[1]])
        geom = geom[geom$COUNTYFP %in% str_pad(counties,width=3,side="left",pad="0"),]
      }
      table = agg_table %>%
        mutate(GEOID=cty)
      full = geom
      full@data = data.frame(
        full@data %>% left_join(table,by="GEOID")
      )
      return(full)
    }else{
      stop("'geo_level' must be place, tract, block group, or county")
    }
  }else{
    if(geo_level=="place"){
      states = unique(crosswalk$st)
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
      agg_table$GEOID = as.character(agg_table[,paste0(prefix,"stplc")])
      full = geom
      full@data = data.frame(
        full@data %>% left_join(agg_table,by="GEOID")
      )
      return(full)
    }else if(geo_level=="tract"){
      states = unique(crosswalk$st)
      if(length(states)>1){
        counties = unique(agg_table$county[table$state==states[1]])
        geom = tigris::tracts(state = states[1],county = counties)
        geom = sp::spChFIDs(geom,as.character(1:length(geom)))
        nex = length(geom)+1
        for(i in 2:length(states)){
          counties = unique(agg_table$county[table$state==states[i]])
          next_geom = tigris::tracts(state = states[i],county=counties)
          next_geom = sp::spChFIDs(next_geom,as.character(nex:(nex+length(next_geom)-1)))
          nex = length(next_geom)+1
          geom = maptools::spRbind(geom,next_geom)
        }
      }else{
        counties = unique(agg_table$county[agg_table$state==states[1]])
        geom = tigris::tracts(state = states[1],county = counties)
      }
      agg_table$GEOID = as.character(agg_table[,paste0(prefix,"trct")])
      full = geom
      full@data = data.frame(
        full@data %>% left_join(agg_table,by="GEOID")
      )
      return(full)
    }else if(geo_level=="block group"){
      states = unique(crosswalk$st)
      if(length(states)>1){
        counties = unique(agg_table$county[table$state==states[1]])
        geom = tigris::block_groups(state = states[1],county = counties)
        geom = sp::spChFIDs(geom,as.character(1:length(geom)))
        nex = length(geom)+1
        for(i in 2:length(states)){
          counties = unique(agg_table$county[table$state==states[i]])
          next_geom = tigris::block_groups(state = states[i],county=counties)
          next_geom = sp::spChFIDs(next_geom,as.character(nex:(nex+length(next_geom)-1)))
          nex = length(next_geom)+1
          geom = maptools::spRbind(geom,next_geom)
        }
      }else{
        counties = unique(agg_table$county[agg_table$state==states[1]])
        geom = tigris::block_groups(state = states[1],county = counties)
      }
      agg_table$GEOID = as.character(agg_table[,paste0(prefix,"bgrp")])
      full = geom
      full@data = data.frame(
        full@data %>% left_join(agg_table,by="GEOID")
      )
      return(full)
    }else if(geo_level=="county"){
      states = unique(crosswalk$st)
      if(length(states)>1){
        geom = tigris::counties(state = states[1])
        counties = unique(agg_table$county[table$state==states[1]])
        geom = geom[geom$COUNTYFP %in% str_pad(counties,width=3,side="left",pad="0"),]
        geom = sp::spChFIDs(geom,as.character(1:length(geom)))
        nex = length(geom)+1
        for(i in 2:length(states)){
          next_geom = tigris::counties(state = states[i])
          counties = unique(agg_table$county[agg_table$state==states[i]])
          next_geom = next_geom[next_geom$COUNTYFP %in% str_pad(counties,width=3,side="left",pad="0"),]
          next_geom = sp::spChFIDs(next_geom,as.character(nex:(nex+length(next_geom)-1)))
          nex = length(next_geom)+1
          geom = maptools::spRbind(geom,next_geom)
        }
      }else{
        geom = tigris::counties(state = states[1])
        counties = unique(agg_table[table$state==states[1],which(grepl())])
        geom = geom[geom$COUNTYFP %in% str_pad(counties,width=3,side="left",pad="0"),]
      }
      agg_table$GEOID = as.character(agg_table[,paste0(prefix,"cty")])
      full = geom
      full@data = data.frame(
        full@data %>% left_join(agg_table,by="GEOID")
      )
      return(full)
    }else{
      stop("'geo_level' must be place, tract, block group, or county")
    }
  }
}
