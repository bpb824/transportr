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
