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