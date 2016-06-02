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
  data = as_data_frame(data.frame(result@estimate,check.names = FALSE))

  base_geog = result@geography
  names = base_geog$NAME

  results = tbl_df(base_geog)

  results$total_pop = data$`Total Population: Total`
  print(paste0(round(1/8*100,0),"% done with queries"))

  #Poverty
  tid = "C17002"
  result = acs.fetch(endyear = endyear,span=span,geography=geog,table.number =tid,col.names = "pretty")
  data = as_data_frame(data.frame(result@estimate,check.names = FALSE))
  sub = data[,1:5]
  props = sub %>% dplyr::rename(total=`Ratio of Income to Poverty Level in the Past 12 Months: Total:`) %>%
    dplyr::mutate(below_150= `Ratio of Income to Poverty Level in the Past 12 Months: Under .50`+
             `Ratio of Income to Poverty Level in the Past 12 Months: .50 to .99`+
             `Ratio of Income to Poverty Level in the Past 12 Months: 1.00 to 1.24`+
             `Ratio of Income to Poverty Level in the Past 12 Months: 1.25 to 1.49`) %>%
    dplyr::select(total,below_150) %>% dplyr::mutate(below_150_prop= below_150/total)
  results$poverty = props$below_150_prop
  print(paste0(round(2/8*100,0),"% done with queries"))

  #Older Adults
  tid = "B01001"
  result = acs.fetch(endyear = endyear,span=span,geography=geog,table.number =tid,col.names = "pretty")
  data = as_data_frame(data.frame(result@estimate,check.names = FALSE))
  sub = data %>% dplyr::select(contains("Total"),contains("65"), contains("67"),
                               contains("70"),contains("75"),contains("80"),
                               contains("85"))
  sub$geography = names
  props= sub %>% dplyr::rename(total=`Sex by Age: Total:`) %>%
    tidyr::gather(key,value,-geography,-total) %>% dplyr::select(-key) %>%
    dplyr::group_by(geography,total) %>% dplyr::summarise(old = sum(value)) %>% dplyr::mutate(old_prop = old/total)

  results$older_adults = props$old_prop
  print(paste0(round(3/8*100,0),"% done with queries"))

  #Youth 10-17
  tid = "B01001"
  result = acs.fetch(endyear = endyear,span=span,geography=geog,table.number =tid,col.names = "pretty")
  data = as_data_frame(data.frame(result@estimate,check.names = FALSE))
  sub = data %>% dplyr::select(contains("Total"), contains("10"), contains("17"))
  sub$geography = names
  props= sub %>% dplyr::rename(total=`Sex by Age: Total:`) %>%
    tidyr::gather(key,value,-geography,-total) %>% dplyr::select(-key) %>%
    dplyr::group_by(geography,total) %>% dplyr::summarise(young = sum(value)) %>% dplyr::mutate(young_prop = young/total)

  results$youth = props$young_prop
  print(paste0(round(4/8*100,0),"% done with queries"))

  #College Age 18-24
  tid = "B01001"
  result = acs.fetch(endyear = endyear,span=span,geography=geog,table.number =tid,col.names = "pretty")
  data = as_data_frame(data.frame(result@estimate,check.names = FALSE))
  sub = data %>% dplyr::select(contains("Total"), contains("18"), contains("20"),
                        contains("21"),contains("22"))
  sub$geography = names
  props= sub %>% dplyr::rename(total=`Sex by Age: Total:`) %>%
    tidyr::gather(key,value,-geography,-total) %>% dplyr::select(-key) %>%
    group_by(geography,total) %>% dplyr::summarise(young = sum(value)) %>% dplyr::mutate(young_prop = young/total)

  results$college_age = props$young_prop
  print(paste0(round(5/8*100,0),"% done with queries"))

  #Persons with disabilities
  tid = "B18101"
  result = acs.fetch(endyear = endyear,span=span,geography=geog,table.number =tid,col.names = "pretty")
  data = as_data_frame(data.frame(result@estimate,check.names = FALSE))
  sub = data %>% dplyr::select(contains("Total"), contains("With a disability"))
  sub$geography = names
  props= sub %>% dplyr::rename(total=`Sex by Age by Disability Status: Total:`) %>%
    tidyr::gather(key,value,-geography,-total) %>% dplyr::select(-key) %>%
    group_by(geography,total) %>% dplyr::summarise(disabled = sum(value)) %>% dplyr::mutate(disabled_prop = disabled/total)

  results$disabled = props$disabled_prop
  print(paste0(round(6/8*100,0),"% done with queries"))

  #Households without Vehicles
  tid = "B08201"
  result = acs.fetch(endyear = endyear,span=span,geography=geog,table.number =tid,col.names = "pretty")
  data = as_data_frame(data.frame(result@estimate,check.names = FALSE))
  sub = data %>% dplyr::select(contains("Total"), contains("No vehicle available"))
  sub$geography = names
  props= sub %>% dplyr::rename(total=`HOUSEHOLD SIZE BY VEHICLES AVAILABLE: Total:`) %>%
    tidyr::gather(key,value,-geography,-total) %>% dplyr::select(-key) %>%
    group_by(geography,total) %>% dplyr::summarise(zero_vehicles = sum(value)) %>% dplyr::mutate(zero_vehicles_prop = zero_vehicles/total)

  results$no_vehicles = props$zero_vehicles_prop
  print(paste0(round(7/8*100,0),"% done with queries"))

  #Limited English
  tid = "B16004"
  result = acs.fetch(endyear = endyear,span=span,geography=geog,table.number =tid,col.names = "pretty")
  data = as_data_frame(data.frame(result@estimate,check.names = FALSE))
  sub = data %>% dplyr::select(contains("Total"), contains("not well"),contains("not at all"))
  sub$geography = names
  props= sub %>% dplyr::rename(total=`Age by Language Spoken at Home by Ability to Speak English for the Population 5+ Yrs: Total:`) %>%
    tidyr::gather(key,value,-geography,-total) %>% dplyr::select(-key) %>%
    group_by(geography,total) %>% dplyr::summarise(no_english = sum(value)) %>% dplyr::mutate(no_english_prop = no_english/total)

  results$non_english = props$no_english_prop
  print(paste0(round(8/8*100,0),"% done with queries"))

  return(results)
}
