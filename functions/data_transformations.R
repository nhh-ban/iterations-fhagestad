# Problem 2
# - - - - - - - - - - - - - - - - -
unlist_safe <- 
  function(x){
    x <- unlist(x)
    if(is.null(x)){
      return(NA_character_)
    }else{
      return(x)
    }
    
# Transforming
 transform_metadata_to_df <- 
  function(df) {
    df <-
      stations_metadata[[1]] %>%
      map(as_tibble) %>% 
      list_rbind() %>% 
      mutate(latestData = map_chr(latestData, unlist_safe)) %>% 
      mutate(latestData = as_datetime(latestData, tz = "UTC")) %>% 
      unnest_wider(location) %>% 
      unnest_wider(latLon)
    
    return(df) 
  }   

# Problem 4 
# - - - - - - - - - - - -
# Time period function
to_iso8601 <- function(datetime, offset_days) {
  
datetime_var <- anytime(datetime)
   
# Offset measured in days
adjusted_datetime <- datetime_var + days(offset_days)
   
# Formatting with Z to indicate time zone "UTC"
iso8601_z <- format(adjusted_datetime, 
                    format = "%Y-%m-%dT%H:%M:%SZ",
                    tz = "UTC")
return(iso8601_z)
 }
 
# Test the time function
to_iso8601(as_datetime("2016-09-01 10:11:12"), 0)   

# Problem 5
# - - - - - - - - - - 

transform_volumes <- function(json_data) {
 if(is.character(json_data)) {
   json_data <- fromJSON(json_data)
  }

edges <- json_data$trafficData$volume$byHour$edges
  
# Converting into a dataframe
volumes_df <- tibble(
  from = map_chr(edges, ~ .x$node$from) %>% ymd_hms(),
  to = map_chr(edges, ~ .x$node$to) %>% ymd_hms(),
  volume = map_dbl(edges, ~ .x$node$total$volumeNumbers$volume)
  )
return(volumes_df)
}
 

 
 
 
 
 
 
 