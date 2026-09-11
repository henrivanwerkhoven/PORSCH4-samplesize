# load latest simulation with the right FU time from the cache
f <- "cache" %>%
  list.files(
    pattern=cache_filename %>% 
      # replace date by date pattern
      str_replace("[0-9]{4}-[0-9]{2}-[0-9]{2}", "[0-9]{4}-[0-9]{2}-[0-9]{2}"), 
    full.names = TRUE) %>%
  sort() %>%
  last()
if(!is.na(f)){
  message("Loading ", f)
  scenarios <- readRDS(f)
}else{
  message("No files found in cache")
}
